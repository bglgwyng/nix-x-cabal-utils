import Control.Arrow ((&&&))
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Maybe
import Distribution.Client.IndexUtils (getInstalledPackages)
import Distribution.Client.IndexUtils.ActiveRepos (CombineStrategy (..))
import Distribution.Client.SolverInstallPlan
import Distribution.Client.Types
import Distribution.PackageDescription (unFlagAssignment)
import Distribution.Simple
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.Program
import Distribution.Solver.Types.PackageIndex qualified as PackageIndex
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage
import Distribution.System (buildPlatform)
import Distribution.Verbosity
import Hackage.Security.Client
import Hackage.Security.Util.Path
import NixXCabal.PackageSet
import NixXCabal.PackagesConfig
import NixXCabal.ReposConfig
import NixXCabal.Repository
import NixXCabal.Resolve (resolvePackagesWithResolution)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { ghcPath :: FilePath
  , ghcPkgPath :: FilePath
  , packagesConfigPath :: FilePath
  , reposConfigPath :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "ghc"
          <> metavar "PATH"
          <> help "Path to ghc"
      )
    <*> strOption
      ( long "ghc-pkg"
          <> metavar "PATH"
          <> help "Path to ghc-pkg"
      )
    <*> strOption
      ( long "packages-config"
          <> metavar "PATH"
          <> help "Path to the packages configuration"
      )
    <*> strOption
      ( long "repos-config"
          <> metavar "PATH"
          <> help "Path to the repositories configuration"
      )

main :: IO ()
main = do
  Options
    { ghcPath
    , ghcPkgPath
    , packagesConfigPath
    , reposConfigPath
    } <-
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Generate a package set")

  config <- readPackagesConfig packagesConfigPath
  indexConfig <- readReposConfig reposConfigPath
  (installed, compiler) <- do
    (compiler, _, programDb) <- GHC.configure normal (Just ghcPath) (Just ghcPkgPath) defaultProgramDb
    installed <- getInstalledPackages normal compiler [GlobalPackageDB] programDb
    pure (installed, compilerInfo compiler)
  sourceDbs <-
    M.fromList
      <$> traverse (\(name, config') -> do db <- readRepositoryIndex name config'; pure (name, db)) (M.assocs indexConfig.repositories)
  source <- either fail pure (combineSourcePackageDbs indexConfig.activeRepositories sourceDbs)

  case resolvePackagesWithResolution
    buildPlatform
    compiler
    installed
    source
    config.packages of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right result -> do
      let packageIds = packageId <$> result
      metadata <-
        M.fromList
          <$> traverse
            ( \(repository, config) -> do
                repositoryMetadata <- readPackageMetadata (repository, config) packageIds
                pure (repository, repositoryMetadata)
            )
            (M.assocs indexConfig.repositories)

      BL.putStr $
        encode $
          PackageSet
            { ghc = ghcPath
            , ghcPkg = ghcPkgPath
            , reposConfig = reposConfigPath
            , packages = M.fromList $ (((.pkgName) . packageId) &&& packageToEntry metadata config.packages) <$> result
            }

packageToEntry :: M.Map RepoName RepositoryPackageMetadata -> M.Map PackageName PackageConfig -> ResolverPackage UnresolvedPkgLoc -> Maybe PackageEntry
packageToEntry _ _ PreExisting{} = Nothing
packageToEntry metadata packages (Configured package) =
  Just $
    PackageEntry
      { version = packageId'.pkgVersion
      , repository = repository'
      , source = case sourceMetadata of
          Nothing -> LocalSource{localPath = url}
          Just metadata' ->
            RemoteSource
              { remoteUrl = url
              , remoteHash = metadata'.sourceHash
              , remoteEditedCabal = metadata'.editedCabal
              }
      , flags = M.fromList $ unFlagAssignment package.solverPkgFlags
      , jailbreak = maybe False (not . null . (.allowNewer)) (M.lookup (packageName packageId') packages)
      }
 where
  packageId' = (packageId package)
  PackageMetadata{sourceMetadata} =
    (metadata M.! repository') M.! packageName packageId'
  repository' =
    fromMaybe
      (error "package does not come from a repository")
      (repositoryName package.solverPkgSource.srcpkgSource)
  (_, url) = case package.solverPkgSource.srcpkgSource of
    RepoTarballPackage (RepoSecure repo' _) _ _ ->
      let Path url =
            anchorRepoPathRemotely
              (Path (show repo'.remoteRepoURI))
              (repoLayoutPkgTarGz hackageRepoLayout packageId')
       in (repo'.remoteRepoName, Path url)
    RepoTarballPackage (RepoLocalNoIndex localRepo _) _ _ ->
      (localRepo.localRepoName, Path $ localRepo.localRepoPath <> "/" <> unPackageName (packageName packageId'))
    _ -> error "not supported"

combineSourcePackageDbs :: [ActiveRepository] -> M.Map RepoName SourcePackageDb -> Either String SourcePackageDb
combineSourcePackageDbs [] _ = Left "active-repositories must contain at least one repository"
combineSourcePackageDbs (ActiveRepository name _ : rest) dbs = do
  initial <- lookupRepository name
  foldl' combine (Right initial) rest
 where
  lookupRepository name = maybe (Left ("active repository is not configured: " <> show name)) Right (M.lookup name dbs)
  combine result (ActiveRepository name mode) = do
    current <- result
    next <- lookupRepository name
    pure $ combineSourcePackageDbs' mode current next
  combineSourcePackageDbs' :: CombineStrategy -> SourcePackageDb -> SourcePackageDb -> SourcePackageDb
  combineSourcePackageDbs' mode current next =
    SourcePackageDb
      { packageIndex = mergeIndexes mode (current.packageIndex) (next.packageIndex)
      , packagePreferences = next.packagePreferences `M.union` current.packagePreferences
      }
   where
    mergeIndexes CombineStrategyMerge = PackageIndex.merge
    mergeIndexes CombineStrategyOverride = PackageIndex.override
    mergeIndexes CombineStrategySkip = error "skip is not valid for an active repository"
