module NixXCabal.Resolve (Resolution (..), resolvePackagesWithResolution, syntheticRootPackageName) where

import Data.Foldable
import Data.Function
import Data.List (intercalate)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Distribution.Client.Dependency (PackageSpecifier (NamedPackage), addConstraints, foldProgress, removeLowerBounds, removeUpperBounds, resolveDependencies, setSolveExecutables, standardInstallPolicy)
import Distribution.Client.SolverInstallPlan hiding (toList)
import Distribution.Client.SolverInstallPlan qualified as SolverInstallPlan
import Distribution.Client.Types (SourcePackageDb (..))
import Distribution.Client.Types.AllowNewer
  ( AllowNewer (..),
    AllowOlder (..),
    RelaxDepScope (..),
    RelaxDeps (..),
    RelaxedDep (..),
  )
import Distribution.Client.Types.PackageLocation (PackageLocation (LocalUnpackedPackage), UnresolvedPkgLoc)
import Distribution.Compat.NonEmptySet (singleton)
import Distribution.Compiler (CompilerInfo)
import Distribution.Package (PackageIdentifier (..), PackageName, mkPackageName, packageId, packageName, pkgVersion)
import Distribution.PackageDescription (Dependency (..), LibraryName (LMainLibName), PackageDescription (..), emptyPackageDescription, mkFlagAssignment)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (ConstraintSourceConfigFlagOrTarget))
import Distribution.Solver.Types.LabeledPackageConstraint (LabeledPackageConstraint (LabeledPackageConstraint))
import Distribution.Solver.Types.PackageConstraint (PackageConstraint (PackageConstraint), PackageProperty (PackagePropertyFlags), scopeToplevel)
import Distribution.Solver.Types.PackageIndex qualified as PackageIndex
import Distribution.Solver.Types.PkgConfigDb (pkgConfigDbFromList)
import Distribution.Solver.Types.Settings (SolveExecutables (SolveExecutables))
import Distribution.Solver.Types.SolverId (solverSrcId)
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform)
import Distribution.Types.BuildInfo (BuildInfo (..), emptyBuildInfo)
import Distribution.Types.CondTree (CondTree (CondNode))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..), emptyGenericPackageDescription)
import Distribution.Types.Library (Library (..), emptyLibrary)
import Distribution.Types.SetupBuildInfo (SetupBuildInfo (..))
import Distribution.Version (Version, anyVersion, mkVersion)
import NixXCabal.PackagesConfig (PackageConfig (..))

data Resolution = Resolution
  { packages :: [ResolverPackage UnresolvedPkgLoc],
    libraryVersions :: M.Map PackageName Version
  }

resolvePackagesWithResolution :: Platform -> CompilerInfo -> InstalledPackageIndex -> SourcePackageDb -> M.Map PackageName PackageConfig -> Either String Resolution
resolvePackagesWithResolution platform compiler installed source packages =
  case foldProgress (\_ rest -> rest) Left Right (resolveDependencies platform compiler (Just $ pkgConfigDbFromList []) params) of
    Left err -> Left err
    Right plan ->
      let resolvedPackages = SolverInstallPlan.toList plan
          closure = libraryClosure resolvedPackages
       in case packageIdsWithMultipleInstances resolvedPackages of
            [] ->
              Right
                Resolution
                  { packages = filter ((/= syntheticRootPackageName) . packageName . packageId) resolvedPackages,
                    libraryVersions =
                      M.fromList
                        [ (packageName packageId', pkgVersion packageId')
                        | packageId' <- S.toList closure,
                          packageName packageId' /= syntheticRootPackageName
                        ]
                  }
            packageIds ->
              Left $
                "package/version appears more than once in the resolved install plan: "
                  <> intercalate ", " (prettyShow <$> packageIds)
                  <> ". Patch dependency bounds or flags so the solver selects a single instance."
  where
    requested = [NamedPackage syntheticRootPackageName []]
    flagConstraints =
      [ LabeledPackageConstraint
          (PackageConstraint (scopeToplevel name) (PackagePropertyFlags . mkFlagAssignment . M.toList $ config.flags))
          ConstraintSourceConfigFlagOrTarget
      | (name, config) <- M.toList packages
      , not (M.null config.flags)
      ]
    params =
      standardInstallPolicy installed (sourceWithSyntheticRoot source) requested
        & addConstraints flagConstraints
        & removeUpperBounds (AllowNewer (RelaxDepsSome packageAllowNewer))
        & removeLowerBounds (AllowOlder (RelaxDepsSome packageAllowOlder))
        & setSolveExecutables (SolveExecutables True)

    syntheticRootId = PackageIdentifier syntheticRootPackageName (mkVersion [0, 0, 0])
    sourceWithSyntheticRoot sourceDb =
      sourceDb
        { packageIndex =
            PackageIndex.insert syntheticRoot sourceDb.packageIndex
        }
    syntheticRoot =
      SourcePackage
        { srcpkgPackageId = syntheticRootId,
          srcpkgDescription = syntheticDescription,
          srcpkgSource = LocalUnpackedPackage ".",
          srcpkgDescrOverride = Nothing
        }
    syntheticDescription =
      emptyGenericPackageDescription
        { packageDescription =
            emptyPackageDescription
              { package = syntheticRootId,
                library = Just syntheticLibrary
              },
          condLibrary = Just (CondNode syntheticLibrary [] [])
        }
    syntheticLibrary =
      emptyLibrary
        { libBuildInfo =
            emptyBuildInfo
              { targetBuildDepends =
                  [ Dependency
                      name
                      (fromMaybe anyVersion config.version)
                      (singleton LMainLibName)
                  | (name, config) <- M.toList packages
                  ]
              }
        }

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

    libraryClosure resolvedPackages = go mempty [syntheticRootId]
      where
        byId = M.fromList [(packageId package, package) | package <- resolvedPackages]
        go seen [] = seen
        go seen (packageId' : rest)
          | packageId' `S.member` seen = go seen rest
          | otherwise =
              let dependencies = case M.lookup packageId' byId of
                    Just (Configured package) ->
                      [ solverSrcId dependency
                      | dependency <- concat (toList package.solverPkgLibDeps),
                        packageName (solverSrcId dependency) `notElem` setupDependencyNames package
                      ]
                    _ -> []
               in go (S.insert packageId' seen) (dependencies <> rest)

        setupDependencyNames package =
          maybe [] (map dependencyName . setupDepends) package.solverPkgSource.srcpkgDescription.packageDescription.setupBuildInfo
        dependencyName (Dependency name _ _) = name

syntheticRootPackageName :: PackageName
syntheticRootPackageName = mkPackageName "nix-x-cabal-utils-root"

packageIdsWithMultipleInstances :: [ResolverPackage UnresolvedPkgLoc] -> [PackageIdentifier]
packageIdsWithMultipleInstances packages =
  [ packageId'
  | (packageId', count) <- M.toList counts,
    count > 1
  ]
  where
    counts = M.fromListWith (+) [(packageId package, 1 :: Int) | package <- packages]
