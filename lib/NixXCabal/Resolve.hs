module NixXCabal.Resolve (resolvePackages, resolvePackagesWithResolution) where

import Data.Maybe (mapMaybe)
import Distribution.Client.Dependency (PackageSpecifier (NamedPackage), addConstraints, foldProgress, removeLowerBounds, removeUpperBounds, resolveDependencies, standardInstallPolicy)
import Distribution.Client.SolverInstallPlan qualified as SolverInstallPlan
import Distribution.Client.Types (SourcePackageDb)
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..), RelaxDeps (RelaxDepsSome))
import Distribution.Client.Types.PackageLocation (UnresolvedPkgLoc)
import Distribution.Compiler (CompilerInfo)
import Distribution.Package (PackageName)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Solver.Types.PkgConfigDb (pkgConfigDbFromList)
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform)
import NixXCabal.PackagesConfig (ResolutionConfig (..))

resolvePackages :: Platform -> CompilerInfo -> InstalledPackageIndex -> SourcePackageDb -> [PackageName] -> Either String [SourcePackage UnresolvedPkgLoc]
resolvePackages platform compiler installed source names = resolvePackagesWithResolution emptyResolution platform compiler installed source names

emptyResolution :: ResolutionConfig
emptyResolution = ResolutionConfig (AllowNewer (RelaxDepsSome [])) (AllowOlder (RelaxDepsSome [])) []

resolvePackagesWithResolution :: ResolutionConfig -> Platform -> CompilerInfo -> InstalledPackageIndex -> SourcePackageDb -> [PackageName] -> Either String [SourcePackage UnresolvedPkgLoc]
resolvePackagesWithResolution resolution platform compiler installed source names =
  case foldProgress (\_ rest -> rest) Left Right (resolveDependencies platform compiler (Just $ pkgConfigDbFromList []) params) of
    Left err -> Left err
    Right plan -> Right (mapMaybe sourceOfConfigured (SolverInstallPlan.toList plan))
 where
  requested = [NamedPackage name [] | name <- names]
  params =
    addConstraints
      (constraints resolution)
      ( removeLowerBounds
          (allowOlder resolution)
          ( removeUpperBounds
              (allowNewer resolution)
              (standardInstallPolicy installed source requested)
          )
      )
  sourceOfConfigured (SolverInstallPlan.Configured solverPkg) =
    Just (solverPkgSource solverPkg)
  sourceOfConfigured (SolverInstallPlan.PreExisting _) = Nothing
