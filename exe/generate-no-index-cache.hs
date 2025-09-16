{-# OPTIONS_GHC -Wno-missing-fields #-}

import Distribution.Client.GlobalFlags (RepoContext (..))
import Distribution.Client.IndexUtils (Index (RepoIndex), updateRepoIndexCache)
import Distribution.Client.Types
import Distribution.Verbosity
import System.Environment

main :: IO ()
main = do
  (path : _) <- getArgs
  updateRepoIndexCache
    silent
    ( RepoIndex
        (RepoContext{})
        ( RepoLocalNoIndex
            { repoLocal = LocalRepo{localRepoPath = path}
            , repoLocalDir = path
            }
        )
    )
