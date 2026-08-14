{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.Repository (
  readHackageIndex,
  buildIndex,
  readPackageHashes,
  readPackageCabals,
  readPackageMetadata,
)
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Codec.Archive.Tar.Index qualified as TarIndex
import Data.Aeson (FromJSON (..), decode, withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Distribution.Client.GlobalFlags (RepoContext (..))
import Distribution.Client.IndexUtils (Index (RepoIndex), getSourcePackages, updateRepoIndexCache)
import Distribution.Client.Types (SourcePackageDb)
import Distribution.Client.Types.Repo (RemoteRepo (..), Repo (RepoSecure))
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Nixpkgs.Hashes (printSHA256)
import Distribution.Package (PackageId)
import Distribution.Pretty (prettyShow)
import Distribution.Verbosity (normal)
import Hackage.Security.Client (hackageIndexLayout, hackageRepoLayout, indexLayoutPkgCabal)
import Hackage.Security.Client qualified as Sec
import Hackage.Security.Client.Repository.Cache qualified as Sec
import Hackage.Security.Client.Repository.Remote (defaultRepoOpts, withRepository)
import Hackage.Security.Util.Path qualified as Sec
import Network.URI (parseURI)
import NixXCabal.ReposConfig (RepositoryConfig (..))
import System.FilePath (takeFileName)

readHackageIndex :: RepositoryConfig -> IO SourcePackageDb
readHackageIndex repoConfig = getSourcePackages normal =<< localRepoContext repoConfig

localRepoContext :: RepositoryConfig -> IO RepoContext
localRepoContext repoConfig = do
  let indexDir = repoConfig.cacheDirectory
      cacheFn = Sec.rootPath . Sec.fragment
      cache =
        Sec.Cache
          (Sec.Path indexDir)
          ( Sec.cabalCacheLayout
              { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
              , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
              , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
              }
          )
  uri <- maybe (fail "invalid repository URI") pure (parseURI (url repoConfig))
  let remote =
        RemoteRepo
          { remoteRepoName = RepoName (repoConfig.name)
          , remoteRepoURI = uri
          , remoteRepoSecure = Just True
          , remoteRepoRootKeys = []
          , remoteRepoKeyThreshold = 0
          , remoteRepoShouldTryHttps = True
          }
  pure
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
      , repoContextRepos = [RepoSecure remote indexDir]
      , repoContextGetTransport = pure (error "HTTP should not be used")
      , repoContextIgnoreExpiry = True
      }

buildIndex :: FilePath -> IO ()
buildIndex indexDir = do
  let cacheFn = Sec.rootPath . Sec.fragment
      cache =
        Sec.Cache
          (Sec.Path indexDir)
          ( Sec.cabalCacheLayout
              { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
              , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
              , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
              }
          )
      remote =
        RemoteRepo
          { remoteRepoName = RepoName "hackage.haskell.org"
          , remoteRepoURI = maybe (error "invalid repository URI") id (parseURI "https://hackage.haskell.org/")
          , remoteRepoSecure = Just True
          , remoteRepoRootKeys = []
          , remoteRepoKeyThreshold = 0
          , remoteRepoShouldTryHttps = True
          }
      repoContext =
        RepoContext
          { repoContextWithSecureRepo = \_ f ->
              withRepository (error "HTTP should not be used") [] defaultRepoOpts cache hackageRepoLayout hackageIndexLayout (\_ -> pure ()) f
          , repoContextRepos = []
          , repoContextGetTransport = pure (error "HTTP should not be used")
          , repoContextIgnoreExpiry = True
          }
      index = RepoIndex repoContext (RepoSecure remote indexDir)
  rebuildTarIndex indexDir
  updateRepoIndexCache normal index

rebuildTarIndex :: FilePath -> IO ()
rebuildTarIndex indexDir = do
  entries <- Tar.read <$> BL.readFile tarPath
  tarIndex <- either (fail . show) pure (TarIndex.build entries)
  BS.writeFile indexPath (TarIndex.serialise tarIndex)
 where
  tarPath = indexDir <> "/01-index.tar"
  indexPath = tarPath <> ".idx"

data PackageJson = PackageJson {signed :: SignedPackageJson}

data SignedPackageJson = SignedPackageJson {targets :: Map.Map String TargetInfo}

data TargetInfo = TargetInfo {hashes :: Hashes}

data Hashes = Hashes {sha256 :: Maybe String}

instance FromJSON PackageJson where
  parseJSON = withObject "PackageJson" $ \o -> PackageJson <$> o .: "signed"

instance FromJSON SignedPackageJson where
  parseJSON = withObject "SignedPackageJson" $ \o -> SignedPackageJson <$> o .: "targets"

instance FromJSON TargetInfo where
  parseJSON = withObject "TargetInfo" $ \o -> TargetInfo <$> o .: "hashes"

instance FromJSON Hashes where
  parseJSON = withObject "Hashes" $ \o -> Hashes <$> o .: "sha256"

readPackageHashes :: FilePath -> IO (Map.Map String String)
readPackageHashes indexTar = do
  entries <- Tar.read <$> BL.readFile indexTar
  let packageJsons = Tar.foldEntries (\entry rest -> if "/package.json" `isSuffixOf` TarEntry.entryPath entry then entry : rest else rest) [] (error . show) entries
  pure (Map.fromList (concatMap packageJsonHashes packageJsons))
 where
  packageJsonHashes entry = case TarEntry.entryContent entry of
    TarEntry.NormalFile contents _ -> maybe [] targetsToHashes (decode contents)
    _ -> []
  targetsToHashes (PackageJson (SignedPackageJson ts)) = mapMaybe targetHash (Map.toList ts)
  targetHash (targetName, TargetInfo (Hashes hashValue)) = do
    packageId <- stripSuffix ".tar.gz" (takeFileName targetName)
    hashValue' <- hashValue
    pure (packageId, hashValue')
  stripSuffix suffix value
    | suffix `isSuffixOf` value = Just (take (length value - length suffix) value)
    | otherwise = Nothing

readPackageCabals :: FilePath -> [PackageId] -> IO (Map.Map PackageId (Int, BL.ByteString))
readPackageCabals indexTar packageIds = do
  entries <- Tar.read <$> BL.readFile indexTar
  let wanted = Map.fromList [(cabalPath packageId, packageId) | packageId <- packageIds]
  pure $ Tar.foldEntries (collect wanted) Map.empty (error . show) entries
 where
  collect wanted entry found =
    case Map.lookup (TarEntry.entryPath entry) wanted of
      Just packageId ->
        case TarEntry.entryContent entry of
          TarEntry.NormalFile contents _ ->
            Map.insertWith
              (\(_, latestContents) (revision, _) -> (revision + 1, latestContents))
              packageId
              (0, contents)
              found
          _ -> found
      Nothing -> found

  cabalPath packageId =
    let Sec.Path path = indexLayoutPkgCabal hackageIndexLayout packageId
     in path

readPackageMetadata :: (RepoName, RepositoryConfig) -> [PackageId] -> IO (Map.Map (RepoName, PackageId) (Maybe String, Maybe (Int, String), BL.ByteString))
readPackageMetadata (repository, repoConfig) packageIds = do
  let indexTar = cacheDirectory repoConfig <> "/01-index.tar"
  hashes <- readPackageHashes indexTar
  cabals <- readPackageCabals indexTar packageIds
  pure
    ( Map.fromList
        [ ((repository, packageId), (Map.lookup (prettyShow packageId) hashes, revisedCabalMetadata cabal, cabalContents))
        | packageId <- packageIds
        , Just cabal@(revision, cabalContents) <- [Map.lookup packageId cabals]
        ]
    )
 where
  revisedCabalMetadata (revision, contents)
    | revision == 0 = Nothing
    | otherwise = Just (revision, printSHA256 (BL.toStrict contents))
