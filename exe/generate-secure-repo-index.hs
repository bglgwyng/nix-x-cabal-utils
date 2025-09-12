{-# OPTIONS_GHC -Wno-missing-fields #-}

import Distribution.Client.IndexUtils
import Distribution.Client.Setup
import Distribution.Client.Types
import Distribution.Verbosity
import Hackage.Security.Client
import Hackage.Security.Client qualified as Sec
import Hackage.Security.Client.Repository.Cache
import Hackage.Security.Client.Repository.Cache qualified as Sec
import Hackage.Security.Client.Repository.Remote
import Hackage.Security.Util.Path qualified as Sec
import System.Environment

main :: IO ()
main = do
  (repoName : path : _) <- getArgs

  let cache :: Sec.Cache
      cache =
        Sec.Cache
          { cacheRoot = Sec.Path path
          , cacheLayout =
              Sec.cabalCacheLayout
                { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
                , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
                , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
                }
          }

      cacheFn :: FilePath -> Sec.CachePath
      cacheFn = Sec.rootPath . Sec.fragment
  let repoContext =
        RepoContext
          { repoContextWithSecureRepo = \_ f -> do
              withRepository
                (error "HTTP should not be used. Please report a bug if you see this.")
                []
                defaultRepoOpts
                cache
                hackageRepoLayout
                hackageIndexLayout
                (\_ -> pure ())
                f
          , repoContextRepos = []
          , repoContextGetTransport = pure (error "hi")
          , repoContextIgnoreExpiry = True
          }
      repo =
        RepoSecure
          { repoRemote = RemoteRepo{remoteRepoName = RepoName repoName}
          , repoLocalDir = path
          }
  rebuildTarIndex cache
  updateRepoIndexCache
    silent
    ( RepoIndex
        repoContext
        ( RepoSecure
            { repoRemote = repoRemote repo
            , repoLocalDir = path
            }
        )
    )