import Distribution.System (buildPlatform)
import NixXCabal.NixOutput (writeNixExpressions)
import Options.Applicative

data Options = Options
  { packageSetPath :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "package-set"
          <> metavar "PATH"
          <> help "Path to package-set.json"
      )

main :: IO ()
main = do
  Options{..} <-
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Generate a packages.nix")

  writeNixExpressions packageSetPath buildPlatform
