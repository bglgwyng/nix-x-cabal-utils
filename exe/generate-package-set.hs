import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Distribution.Client.IndexUtils (getInstalledPackages)
import Distribution.Pretty (prettyShow)
import Distribution.Simple
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.Program
import Distribution.System (buildPlatform)
import Distribution.Verbosity
import NixXCabal.Output (resolvedPackageSet)
import NixXCabal.PackageSet (ResolvedPackage (..), sourcePackageId)
import NixXCabal.PackagesConfig
import NixXCabal.ReposConfig
import NixXCabal.Repository
import NixXCabal.Resolve (resolvePackagesWithResolution)
import Options.Applicative
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
  Options{..} <-
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
  repo@(repoName, repoConfig) <- either fail pure (singleRepository (indexConfig.repositories))
  source <- readHackageIndex repoConfig

  case resolvePackagesWithResolution
    (resolution config)
    buildPlatform
    compiler
    installed
    source
    (packages config) of
    Left err -> hPutStrLn stderr err
    Right result -> do
      let packageIds = map sourcePackageId result
      metadata <- readPackageMetadata repo packageIds
      let missing = [packageId | packageId <- packageIds, Map.notMember (repoName, packageId) metadata]
      case missing of
        missingPackageIds@(_ : _) -> fail ("missing .cabal files in repository index: " <> show (map prettyShow missingPackageIds))
        [] -> do
          let resolved =
                [ ResolvedPackage
                    { sourcePackage = sourcePackage'
                    , sourceHash = sourceHash'
                    , sourceRevision = fst <$> revisedCabal
                    , sourceEditedCabalFile = snd <$> revisedCabal
                    }
                | sourcePackage' <- result
                , let packageId = sourcePackageId sourcePackage'
                      (sourceHash', revisedCabal, _) = metadata Map.! (repoName, packageId)
                ]
          BL.putStr
            (encode (resolvedPackageSet ghcPath ghcPkgPath reposConfigPath resolved) <> BL.singleton 10)
