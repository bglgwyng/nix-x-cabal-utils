{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.PackagesConfig (
  PackagesConfig (..),
  PackageConfig (..),
  readPackagesConfig,
)
where

import Data.Aeson (
  FromJSON (..),
  eitherDecode,
  withObject,
  (.!=),
  (.:?),
 )
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.String (fromString)
import Distribution.Client.Types.AllowNewer (
  RelaxDepMod (..),
  RelaxDepSubject (..),
 )
import Distribution.PackageDescription (FlagName)
import Distribution.Simple
import GHC.Generics (Generic)
import NixXCabal.JSON.Orphans ()

data PackagesConfig = PackagesConfig
  { packages :: M.Map PackageName PackageConfig
  }
  deriving (Generic, FromJSON)

data PackageConfig = PackageConfig
  { version :: Maybe VersionRange
  , flags :: M.Map FlagName Bool
  , allowNewer :: [(RelaxDepMod, RelaxDepSubject)]
  , allowOlder :: [(RelaxDepMod, RelaxDepSubject)]
  }
  deriving (Generic)

instance FromJSON PackageConfig where
  parseJSON = withObject "PackageConfig" $ \object -> do
    version <- object .:? "version"
    flags <- object .:? "flags" .!= mempty
    allowNewer <- parseRelaxedDeps object "allow-newer"
    allowOlder <- parseRelaxedDeps object "allow-older"
    pure (PackageConfig version flags allowNewer allowOlder)
   where
    parseRelaxedDeps object key = do
      values <- object .:? key .!= []
      traverse (either fail pure . parseRelaxedDepParts) values

parseRelaxedDepParts :: String -> Either String (RelaxDepMod, RelaxDepSubject)
parseRelaxedDepParts value =
  let (modifier, subjectName) = case value of
        '^' : rest -> (RelaxDepModCaret, rest)
        rest -> (RelaxDepModNone, rest)
   in Right
        ( modifier
        , case subjectName of
            "*" -> RelaxDepSubjectAll
            "all" -> RelaxDepSubjectAll
            subject -> RelaxDepSubjectPkg (fromString subject)
        )

readPackagesConfig :: FilePath -> IO PackagesConfig
readPackagesConfig path = do
  contents <- BL.readFile path
  either fail pure (eitherDecode contents)
