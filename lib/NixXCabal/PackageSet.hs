{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.PackageSet (
  PackageSet (..),
  PackageEntry (..),
  ResolvedPackage (..),
  repositoryName,
  sourcePackageRepository,
  sourcePackageId,
  sourcePackageTarballUrl,
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Map.Strict qualified as Map
import Distribution.Client.Types.PackageLocation (PackageLocation (..), UnresolvedPkgLoc)
import Distribution.Client.Types.Repo (Repo (..), localRepoName, remoteRepoName, remoteRepoURI)
import Distribution.Client.Types.RepoName (RepoName)
import Distribution.Package (PackageId)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Solver.Types.SourcePackage (SourcePackage, srcpkgPackageId, srcpkgSource)
import Distribution.Version (Version)
import Hackage.Security.Client (hackageRepoLayout, repoLayoutPkgTarGz)
import Hackage.Security.Util.Path (Path (..))

data PackageSet = PackageSet
  { ghc :: FilePath
  , ghcPkg :: FilePath
  , reposConfig :: FilePath
  , packages :: Map.Map String PackageEntry
  }

data PackageEntry = PackageEntry
  { version :: Version
  , repository :: Maybe String
  , hash :: Maybe String
  , packageRevision :: Maybe Int
  , editedCabalHash :: Maybe String
  , url :: String
  }

instance FromJSON PackageSet where
  parseJSON = withObject "PackageSet" $ \o ->
    PackageSet <$> o .: "ghc" <*> o .: "ghc-pkg" <*> o .: "repos-config" <*> o .: "packages"

instance FromJSON PackageEntry where
  parseJSON = withObject "PackageEntry" $ \o -> do
    versionText <- o .: "version"
    parsedVersion <- maybe (fail ("invalid package version: " <> versionText)) pure (simpleParsec versionText)
    PackageEntry
      <$> pure parsedVersion
      <*> o .:? "repository"
      <*> o .:? "hash"
      <*> o .:? "revision"
      <*> o .:? "edited-cabal-file"
      <*> o .: "url"

instance ToJSON PackageSet where
  toJSON packageSet =
    object
      [ "ghc" .= packageSet.ghc
      , "ghc-pkg" .= packageSet.ghcPkg
      , "repos-config" .= packageSet.reposConfig
      , "packages" .= packageSet.packages
      ]

instance ToJSON PackageEntry where
  toJSON entry =
    object
      [ "version" .= prettyShow (entry.version)
      , "repository" .= entry.repository
      , "hash" .= entry.hash
      , "revision" .= entry.packageRevision
      , "edited-cabal-file" .= entry.editedCabalHash
      , "url" .= entry.url
      ]

data ResolvedPackage = ResolvedPackage
  { sourcePackage :: SourcePackage UnresolvedPkgLoc
  , sourceHash :: Maybe String
  , sourceRevision :: Maybe Int
  , sourceEditedCabalFile :: Maybe String
  }

repositoryName :: UnresolvedPkgLoc -> Maybe RepoName
repositoryName location = case location of
  RepoTarballPackage repo _ _ -> case repo of
    RepoRemote remote _ -> Just (remoteRepoName remote)
    RepoSecure remote _ -> Just (remoteRepoName remote)
    RepoLocalNoIndex localRepo _ -> Just (localRepoName localRepo)
  _ -> Nothing

sourcePackageRepository :: SourcePackage UnresolvedPkgLoc -> Maybe RepoName
sourcePackageRepository = repositoryName . srcpkgSource

sourcePackageId :: SourcePackage loc -> PackageId
sourcePackageId = srcpkgPackageId

sourcePackageTarballUrl :: SourcePackage UnresolvedPkgLoc -> Maybe String
sourcePackageTarballUrl package = case srcpkgSource package of
  RepoTarballPackage repo packageId _ -> do
    base <- repoUrl repo
    let Path path = repoLayoutPkgTarGz hackageRepoLayout packageId
    pure (base <> path)
  RemoteTarballPackage uri _ -> Just (show uri)
  _ -> Nothing
 where
  repoUrl repo = case repo of
    RepoRemote remote _ -> Just (show (remoteRepoURI remote))
    RepoSecure remote _ -> Just (show (remoteRepoURI remote))
    RepoLocalNoIndex _ _ -> Nothing
