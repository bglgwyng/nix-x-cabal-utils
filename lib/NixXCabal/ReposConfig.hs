{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.ReposConfig (
  ReposConfig (..),
  RepositoryConfig (..),
  readReposConfig,
  singleRepository,
)
where

import Data.Aeson (FromJSON (..), decode, withObject, (.:))
import Data.ByteString.Lazy qualified as BL
import Data.Map (mapKeys)
import Data.Map.Strict qualified as Map
import Distribution.Client.Types

data ReposConfig = ReposConfig
  { repositories :: Map.Map RepoName RepositoryConfig
  }

instance FromJSON ReposConfig where
  parseJSON = withObject "ReposConfig" $ \o -> do
    configured <- mapKeys RepoName <$> o .: "repositories"
    pure (ReposConfig configured)

data RepositoryConfig = RepositoryConfig
  { name :: String
  , type_ :: String
  , url :: String
  , secure :: Bool
  , cacheDirectory :: FilePath
  }

instance FromJSON RepositoryConfig where
  parseJSON = withObject "RepositoryConfig" $ \o ->
    RepositoryConfig
      ""
      <$> o .: "type"
      <*> o .: "url"
      <*> o .: "secure"
      <*> o .: "cache-directory"

singleRepository :: Map.Map RepoName RepositoryConfig -> Either String (RepoName, RepositoryConfig)
singleRepository repos = case Map.assocs repos of
  [] -> Left "configuration must contain one repository"
  [(name, config)]
    | config.type_ /= "remote" -> Left "only remote repositories are supported currently"
    | not (config.secure) -> Left "only secure remote repositories are supported currently"
    | otherwise -> Right (name, config)
  _ -> Left ("multiple repositories are not supported yet; found " <> show (length repos))

readReposConfig :: FilePath -> IO ReposConfig
readReposConfig path = do
  contents <- BL.readFile path
  maybe (fail ("invalid repositories config: " <> path)) pure (decode contents)
