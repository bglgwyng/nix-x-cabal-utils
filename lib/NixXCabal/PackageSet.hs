{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module NixXCabal.PackageSet
  ( PackageSet (..),
    PackageEntry (..),
    PackageSource (..),
    repositoryName,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Map.Strict qualified as M
import Distribution.Client.Types.PackageLocation (PackageLocation (..), UnresolvedPkgLoc)
import Distribution.Client.Types.Repo (Repo (..), localRepoName, remoteRepoName)
import Distribution.Client.Types.RepoName (RepoName)
import Distribution.PackageDescription
import Distribution.Version (Version)
import GHC.Generics (Generic)
import Hackage.Security.Util.Path (Path, Unrooted)
import NixXCabal.JSON.Orphans ()
import NixXCabal.Repository (EditedCabal (..))

data PackageSet = PackageSet
  { ghc :: FilePath,
    ghcPkg :: FilePath,
    reposConfig :: FilePath,
    libraryVersions :: M.Map PackageName Version,
    packages :: [PackageEntry]
  }
  deriving (Generic)

data PackageEntry = PackageEntry
  { packageId :: PackageIdentifier,
    repository :: RepoName,
    source :: PackageSource,
    flags :: M.Map FlagName Bool,
    setupDepends :: M.Map PackageName (Maybe Version),
    jailbreak :: Bool
  }
  deriving (Generic)

data PackageSource
  = LocalSource
      { localPath :: Path Unrooted
      }
  | RemoteSource
      { remoteUrl :: Path Unrooted,
        remoteHash :: String,
        remoteEditedCabal :: Maybe EditedCabal
      }
  deriving (Generic)

instance FromJSON PackageSet where
  parseJSON = withObject "PackageSet" $ \o ->
    PackageSet
      <$> o .: "ghc"
      <*> o .: "ghc-pkg"
      <*> o .: "repos-config"
      <*> o .: "library-versions"
      <*> o .: "packages"

instance FromJSON PackageEntry where
  parseJSON = withObject "PackageEntry" $ \o -> do
    PackageEntry
      <$> o .: "package-id"
      <*> o .: "repository"
      <*> o .: "source"
      <*> o .:? "flags" .!= mempty
      <*> o .:? "setup-depends" .!= mempty
      <*> o .:? "jailbreak" .!= False

instance FromJSON PackageSource where
  parseJSON = withObject "PackageSource" $ \o -> do
    sourceType <- o .: "type"
    case sourceType of
      "local" -> LocalSource <$> o .: "path"
      "remote" -> RemoteSource <$> o .: "url" <*> o .: "hash" <*> o .:? "edited-cabal"
      _ -> fail ("unsupported package source: " <> sourceType)

instance ToJSON PackageSet where
  toJSON packageSet =
    object
      [ "ghc" .= packageSet.ghc,
        "ghc-pkg" .= packageSet.ghcPkg,
        "repos-config" .= packageSet.reposConfig,
        "library-versions" .= packageSet.libraryVersions,
        "packages" .= packageSet.packages
      ]

instance ToJSON PackageEntry where
  toJSON entry =
    object
      [ "package-id" .= entry.packageId,
        "repository" .= entry.repository,
        "source" .= entry.source,
        "flags" .= entry.flags,
        "setup-depends" .= entry.setupDepends,
        "jailbreak" .= entry.jailbreak
      ]

instance ToJSON PackageSource where
  toJSON LocalSource {localPath} = object ["type" .= ("local" :: String), "path" .= localPath]
  toJSON RemoteSource {remoteUrl, remoteHash, remoteEditedCabal} =
    object
      [ "type" .= ("remote" :: String),
        "url" .= remoteUrl,
        "hash" .= remoteHash,
        "edited-cabal" .= remoteEditedCabal
      ]

repositoryName :: UnresolvedPkgLoc -> Maybe RepoName
repositoryName location = case location of
  RepoTarballPackage repo _ _ -> case repo of
    RepoRemote remote _ -> Just remote.remoteRepoName
    RepoSecure remote _ -> Just remote.remoteRepoName
    RepoLocalNoIndex localRepo _ -> Just localRepo.localRepoName
  _ -> Nothing
