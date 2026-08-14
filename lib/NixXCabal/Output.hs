{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.Output (resolvedPackageSet) where

import Data.Map.Strict qualified as Map
import Distribution.Client.Types.RepoName (unRepoName)
import Distribution.Package (pkgName, pkgVersion)
import Distribution.Pretty (prettyShow)
import Distribution.Solver.Types.SourcePackage (srcpkgPackageId)
import NixXCabal.PackageSet (PackageEntry (..), PackageSet (..), ResolvedPackage (..), sourcePackageRepository, sourcePackageTarballUrl)

resolvedPackageSet :: FilePath -> FilePath -> FilePath -> [ResolvedPackage] -> PackageSet
resolvedPackageSet ghcPath ghcPkgPath reposConfigPath resolvedPackages =
  PackageSet
    { ghc = ghcPath
    , ghcPkg = ghcPkgPath
    , reposConfig = reposConfigPath
    , packages = Map.fromList (map resolvedPackageEntry resolvedPackages)
    }
 where
  resolvedPackageEntry package =
    let ResolvedPackage{sourcePackage = sourcePackage'} = package
        packageId' = srcpkgPackageId sourcePackage'
        repositoryName = sourcePackageRepository sourcePackage'
        packageUrl = maybe (error ("missing source URL for " <> prettyShow packageId')) id (sourcePackageTarballUrl sourcePackage')
     in ( prettyShow (pkgName packageId')
        , PackageEntry
            { version = pkgVersion packageId'
            , repository = fmap unRepoName repositoryName
            , hash = sourceHash package
            , packageRevision = sourceRevision package
            , editedCabalHash = sourceEditedCabalFile package
            , url = packageUrl
            }
        )
