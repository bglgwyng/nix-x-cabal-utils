{-# OPTIONS_GHC -Wno-orphans #-}

module NixXCabal.JSON.Orphans () where

import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  FromJSONKeyFunction (..),
  ToJSON (..),
  ToJSONKey (..),
  ToJSONKeyFunction (..),
  withText,
 )
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.String (fromString)
import Data.Text qualified as Text
import Distribution.PackageDescription (FlagName)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageName)
import Distribution.Version (VersionRange)
import Hackage.Security.Util.Path (Path (..), Unrooted)

instance FromJSON (Path Unrooted) where
  parseJSON = withText "Path" (pure . Path . Text.unpack)

instance ToJSON (Path Unrooted) where
  toJSON (Path path) = toJSON path

instance FromJSON VersionRange where
  parseJSON = withText "VersionRange" $ \text ->
    maybe
      (fail ("invalid version range: " <> Text.unpack text))
      pure
      (simpleParsec (Text.unpack text))

instance ToJSON VersionRange where
  toJSON = toJSON . prettyShow

instance FromJSON PackageName where
  parseJSON = withText "PackageName" (pure . fromString . Text.unpack)

instance ToJSON PackageName where
  toJSON = toJSON . prettyShow

instance FromJSONKey PackageName where
  fromJSONKey = FromJSONKeyText (fromString . Text.unpack)

instance ToJSONKey PackageName where
  toJSONKey = ToJSONKeyText (Key.fromText . Text.pack . prettyShow) (Encoding.text . Text.pack . prettyShow)

instance FromJSON FlagName where
  parseJSON = withText "FlagName" (pure . fromString . Text.unpack)

instance ToJSON FlagName where
  toJSON = toJSON . prettyShow

instance FromJSONKey FlagName where
  fromJSONKey = FromJSONKeyText (fromString . Text.unpack)

instance ToJSONKey FlagName where
  toJSONKey = ToJSONKeyText (Key.fromText . Text.pack . prettyShow) (Encoding.text . Text.pack . prettyShow)
