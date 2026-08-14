module NixXCabal.Resolve (resolvePackagesWithResolution) where

import Data.Foldable
import Data.Function
import Data.List (intercalate)
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Distribution.Client.Dependency (PackageProperty (..), PackageSpecifier (NamedPackage), foldProgress, removeLowerBounds, removeUpperBounds, resolveDependencies, setSolveExecutables, standardInstallPolicy)
import Distribution.Client.SolverInstallPlan hiding (toList)
import Distribution.Client.SolverInstallPlan qualified as SolverInstallPlan
import Distribution.Client.Types (SourcePackageDb)
import Distribution.Client.Types.AllowNewer
  ( AllowNewer (..),
    AllowOlder (..),
    RelaxDepScope (..),
    RelaxDeps (..),
    RelaxedDep (..),
  )
import Distribution.Client.Types.PackageLocation (UnresolvedPkgLoc)
import Distribution.Compiler (CompilerInfo)
import Distribution.Package (PackageName, packageName)
import Distribution.PackageDescription (mkFlagAssignment)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Solver.Types.PkgConfigDb (pkgConfigDbFromList)
import Distribution.Solver.Types.Settings (SolveExecutables (SolveExecutables))
import Distribution.System (Platform)
import NixXCabal.PackagesConfig (PackageConfig (..))

resolvePackagesWithResolution :: Platform -> CompilerInfo -> InstalledPackageIndex -> SourcePackageDb -> M.Map PackageName PackageConfig -> Either String [ResolverPackage UnresolvedPkgLoc]
resolvePackagesWithResolution platform compiler installed source packages =
  case foldProgress (\_ rest -> rest) Left Right (resolveDependencies platform compiler (Just $ pkgConfigDbFromList []) params) of
    Left err -> Left err
    Right plan ->
      let result = SolverInstallPlan.toList plan
       in case packageNamesWithMultipleInstances result of
            [] -> Right result
            names' ->
              Left $
                "package appears more than once in the resolved install plan: "
                  <> intercalate ", " (prettyShow <$> names')
  where
    requested =
      [ NamedPackage
          name
          ( [ PackagePropertyFlags . mkFlagAssignment . M.toList $ config.flags
            | not . M.null $ config.flags
            ]
              <> toList (PackagePropertyVersion <$> config.version)
          )
      | (name, config) <- M.toList packages
      ]
    params =
      standardInstallPolicy installed source requested
        & removeUpperBounds (AllowNewer (RelaxDepsSome packageAllowNewer))
        & removeLowerBounds (AllowOlder (RelaxDepsSome packageAllowOlder))
        & setSolveExecutables (SolveExecutables True)

    packageAllowNewer =
      concatMap
        (\(packageName', PackageConfig {allowNewer = newer}) -> packageRelaxedDeps (packageName', newer))
        (M.toList packages)
    packageAllowOlder =
      concatMap
        (\(packageName', PackageConfig {allowOlder = older}) -> packageRelaxedDeps (packageName', older))
        (M.toList packages)

    packageRelaxedDeps (packageName', values) =
      [ RelaxedDep (RelaxDepScopePackage packageName') modifier subject
      | (modifier, subject) <- values
      ]

packageNamesWithMultipleInstances :: [ResolverPackage UnresolvedPkgLoc] -> [PackageName]
packageNamesWithMultipleInstances packages =
  [ name
  | (name, packages') <- Map.toList byName,
    length (packages') > 1
  ]
  where
    byName =
      Map.fromListWith
        (<>)
        [(packageName package, [package]) | package <- packages]
