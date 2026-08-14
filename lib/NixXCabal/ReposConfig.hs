{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.ReposConfig (
  ReposConfig (..),
  ActiveRepository (..),
  RepositoryConfig (..),
  RemoteRepoConfig (..),
  LocalRepoConfig (..),
  readReposConfig,
)
where

import Data.Aeson (FromJSON (..), decode, withObject, withText, (.:), (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.Map (mapKeys)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Distribution.Client.IndexUtils.ActiveRepos (CombineStrategy (..))
import Distribution.Client.Types
import GHC.Generics (Generic)

data ReposConfig = ReposConfig
  { repositories :: Map.Map RepoName RepositoryConfig
  , activeRepositories :: [ActiveRepository]
  }
  deriving (Generic)

data ActiveRepository = ActiveRepository
  { activeRepositoryName :: RepoName
  , activeRepositoryMerge :: CombineStrategy
  }
  deriving (Generic)

instance FromJSON ReposConfig where
  parseJSON = withObject "ReposConfig" $ \o -> do
    configured <- mapKeys RepoName <$> o .: "repositories"
    active <- o .:? "active-repositories"
    selected <- case active of
      Nothing
        | not (Map.null configured) ->
            fail "active-repositories must be specified when repositories are configured"
      Nothing -> pure []
      Just repositories -> pure repositories
    pure (ReposConfig configured selected)

instance FromJSON ActiveRepository where
  parseJSON = withText "active repository" $ \text ->
    case reverse (Text.splitOn ":" text) of
      ["override", name] -> pure (ActiveRepository (RepoName (Text.unpack name)) CombineStrategyOverride)
      [name] -> pure (ActiveRepository (RepoName (Text.unpack name)) CombineStrategyMerge)
      _ -> fail ("invalid active repository: " <> show text)

data RepositoryConfig
  = RemoteRepository RemoteRepoConfig
  | LocalRepository LocalRepoConfig
  deriving (Generic)

data RemoteRepoConfig = RemoteRepoConfig
  { url :: String
  , secure :: Bool
  , cacheDirectory :: FilePath
  }
  deriving (Generic)

data LocalRepoConfig = LocalRepoConfig
  { localPath :: FilePath
  , cacheDirectory :: FilePath
  }
  deriving (Generic)

instance FromJSON RepositoryConfig where
  parseJSON = withObject "RepositoryConfig" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "remote" ->
        RemoteRepository <$> (RemoteRepoConfig <$> o .: "url" <*> o .: "secure" <*> o .: "cache-directory")
      "local" ->
        LocalRepository <$> (LocalRepoConfig <$> o .: "url" <*> o .: "cache-directory")
      unsupported -> fail ("unsupported repository type: " <> unsupported)

readReposConfig :: FilePath -> IO ReposConfig
readReposConfig path = do
  contents <- BL.readFile path
  maybe (fail ("invalid repositories config: " <> path)) pure (decode contents)
