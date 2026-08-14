import Distribution.System (buildPlatform)
import NixXCabal.PackagesNix (writePackagesNix)
import Options.Applicative

data Options = Options
  { nixpkgsPath :: FilePath
  , packageSetPath :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "nixpkgs"
          <> metavar "PATH"
          <> help "Path to the nixpkgs checkout"
      )
    <*> strOption
      ( long "package-set"
          <> metavar "PATH"
          <> help "Path to package-set.json"
      )

main :: IO ()
main = do
  Options{nixpkgsPath, packageSetPath} <-
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Generate a packages.nix")

  writePackagesNix nixpkgsPath packageSetPath buildPlatform
