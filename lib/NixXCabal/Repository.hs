{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.Repository (
  readRepositoryIndex,
  readPackageHashes,
  readPackageCabals,
  PackageMetadata (..),
  RepositoryPackageMetadata,
  SourceMetadata (..),
  EditedCabal (..),
  readPackageMetadata,
)
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Control.Monad (filterM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Distribution.Client.GlobalFlags (RepoContext (..))
import Distribution.Client.IndexUtils (getSourcePackages)
import Distribution.Client.Types (SourcePackageDb)
import Distribution.Client.Types.Repo (LocalRepo (..), RemoteRepo (..), Repo (..))
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Nixpkgs.Hashes (printSHA256)
import Distribution.Package (PackageId, PackageName, packageName)
import Distribution.Pretty (prettyShow)
import Distribution.Verbosity (normal)
import GHC.Generics (Generic)
import Hackage.Security.Client (hackageIndexLayout, hackageRepoLayout, indexLayoutPkgCabal)
import Hackage.Security.Client qualified as Sec
import Hackage.Security.Client.Repository.Cache qualified as Sec
import Hackage.Security.Client.Repository.Remote (defaultRepoOpts, withRepository)
import Hackage.Security.Util.Path qualified as Sec
import Network.URI (parseURI)
import NixXCabal.ReposConfig
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))

data PackageMetadata = PackageMetadata
  { cabalContents :: BL.ByteString
  , sourceMetadata :: Maybe SourceMetadata
  }
  deriving (Generic)

type RepositoryPackageMetadata = M.Map PackageId PackageMetadata

data SourceMetadata = SourceMetadata
  { sourceHash :: String
  , editedCabal :: Maybe EditedCabal
  }
  deriving (Generic)

data EditedCabal = EditedCabal
  { revision :: Int
  , hash :: String
  }
  deriving (Generic, FromJSON, ToJSON)

readRepositoryIndex :: RepoName -> RepositoryConfig -> IO SourcePackageDb
readRepositoryIndex repoName repoConfig = case repoConfig of
  RemoteRepository remoteConfig -> do
    if not $ remoteConfig.secure
      then fail ("repository is not secure: " <> unRepoName repoName)
      else do
        let cacheFn = Sec.rootPath . Sec.fragment
            cache =
              Sec.Cache
                (Sec.Path remoteConfig.cacheDirectory)
                ( Sec.cabalCacheLayout
                    { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
                    , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
                    , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
                    }
                )
        uri <- maybe (fail "invalid repository URI") pure (parseURI remoteConfig.url)
        let remote =
              RemoteRepo
                { remoteRepoName = repoName
                , remoteRepoURI = uri
                , remoteRepoSecure = Just True
                , remoteRepoRootKeys = []
                , remoteRepoKeyThreshold = 0
                , remoteRepoShouldTryHttps = True
                }
        let repoContext =
              RepoContext
                { repoContextWithSecureRepo = \_ f ->
                    withRepository
                      (error "HTTP should not be used")
                      []
                      defaultRepoOpts
                      cache
                      hackageRepoLayout
                      hackageIndexLayout
                      (\_ -> pure ())
                      f
                , repoContextRepos = [RepoSecure remote remoteConfig.cacheDirectory]
                , repoContextGetTransport = pure (error "HTTP should not be used")
                , repoContextIgnoreExpiry = True
                }
        getSourcePackages normal repoContext
  LocalRepository localConfig ->
    let repo = RepoLocalNoIndex (LocalRepo repoName localConfig.localPath False) (localConfig.cacheDirectory)
     in getSourcePackages
          normal
          RepoContext
            { repoContextRepos = [repo]
            , repoContextGetTransport = pure (error "HTTP should not be used")
            , repoContextWithSecureRepo = \_ _ -> error "secure repositories are not used"
            , repoContextIgnoreExpiry = True
            }

data PackageJson = PackageJson {signed :: SignedPackageJson}
  deriving (Generic, FromJSON)

data SignedPackageJson = SignedPackageJson {targets :: M.Map String TargetInfo}
  deriving (Generic, FromJSON)

data TargetInfo = TargetInfo {hashes :: Hashes}
  deriving (Generic, FromJSON)

data Hashes = Hashes {sha256 :: Maybe String}
  deriving (Generic, FromJSON)

readPackageHashes :: FilePath -> IO (M.Map String String)
readPackageHashes indexTar = do
  entries <- Tar.read <$> BL.readFile indexTar
  let packageJsons = Tar.foldEntries (\entry rest -> if "/package.json" `isSuffixOf` TarEntry.entryPath entry then entry : rest else rest) [] (error . show) entries
  pure (M.fromList (concatMap packageJsonHashes packageJsons))
 where
  packageJsonHashes entry = case TarEntry.entryContent entry of
    TarEntry.NormalFile contents _ -> maybe [] targetsToHashes (decode contents)
    _ -> []
  targetsToHashes (PackageJson (SignedPackageJson ts)) = mapMaybe targetHash (M.toList ts)
  targetHash (targetName, TargetInfo (Hashes hashValue)) = do
    packageId <- stripSuffix ".tar.gz" (takeFileName targetName)
    hashValue' <- hashValue
    pure (packageId, hashValue')
  stripSuffix suffix value
    | suffix `isSuffixOf` value = Just (take (length value - length suffix) value)
    | otherwise = Nothing

readPackageCabals :: FilePath -> [PackageId] -> IO (M.Map PackageId (Int, BL.ByteString))
readPackageCabals indexTar packageIds = do
  entries <- Tar.read <$> BL.readFile indexTar
  let wanted = M.fromList [(cabalPath packageId, packageId) | packageId <- packageIds]
  pure $ Tar.foldEntries (collect wanted) M.empty (error . show) entries
 where
  collect wanted entry found =
    case M.lookup (TarEntry.entryPath entry) wanted of
      Just packageId ->
        case TarEntry.entryContent entry of
          TarEntry.NormalFile contents _ ->
            M.insertWith
              (\(_, _) (revision, latestContents) -> (revision + 1, latestContents))
              packageId
              (0, contents)
              found
          _ -> found
      Nothing -> found

  cabalPath packageId =
    let Sec.Path path = indexLayoutPkgCabal hackageIndexLayout packageId
     in path

readPackageMetadata :: (RepoName, RepositoryConfig) -> [PackageId] -> IO RepositoryPackageMetadata
readPackageMetadata (_, LocalRepository localConfig) packageIds = do
  entries <- catMaybes <$> traverse readLocalPackage packageIds
  pure $ M.fromList entries
 where
  readLocalPackage packageId = do
    cabalPath <- findCabalFile (localConfig.localPath) (prettyShow (packageName packageId) <> ".cabal")
    traverse
      ( \path -> do
          contents <- BL.readFile path
          pure
            ( packageId
            , PackageMetadata
                { cabalContents = contents
                , sourceMetadata = Nothing
                }
            )
      )
      cabalPath

  findCabalFile directory fileName = do
    entries <- listDirectory directory
    let paths = (directory </>) <$> entries
    direct <- filterM (\path -> (&& (takeFileName path == fileName)) <$> doesFileExist path) paths
    case direct of
      path : _ -> pure (Just path)
      [] -> do
        subdirectories <- filterM doesDirectoryExist paths
        matches <- traverse (\path -> findCabalFile path fileName) subdirectories
        pure $ case catMaybes matches of
          path : _ -> Just path
          [] -> Nothing
readPackageMetadata (_, RemoteRepository repoConfig) packageIds = do
  let indexTar = repoConfig.cacheDirectory <> "/01-index.tar"
  hashes <- readPackageHashes indexTar
  cabals <- readPackageCabals indexTar packageIds
  pure
    ( M.fromList
        [ ( packageId
          , PackageMetadata
              { cabalContents
              , sourceMetadata = do
                  sourceHash <- M.lookup (prettyShow packageId) hashes
                  let editedCabal =
                        if revision == 0
                          then Nothing
                          else
                            Just
                              EditedCabal
                                { revision
                                , hash = printSHA256 $ SHA256.hash $ BL.toStrict cabalContents
                                }
                  pure SourceMetadata{sourceHash, editedCabal}
              }
          )
        | packageId <- packageIds
        , let (revision, cabalContents) = cabals M.! packageId
        ]
    )
