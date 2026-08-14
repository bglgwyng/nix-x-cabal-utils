{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module NixXCabal.PackagesNix (
  writePackagesNix,
)
where

import Control.Lens (each, over, view, (&), (.~), (^.))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Nixpkgs.Fetch (DerivationSource (..), urlDerivationSource)
import Distribution.Nixpkgs.Haskell.Derivation (Derivation, benchmarkDepends, dependencies, doBenchmark, doCheck, editedCabalFile, extraFunctionArgs, jailbreak, revision, src, testDepends)
import Distribution.Nixpkgs.Haskell.FromCabal (NixpkgsResolver, fromGenericPackageDescription)
import Distribution.Nixpkgs.PackageMap (readNixpkgPackageMap, resolve)
import Distribution.PackageDescription (mkFlagAssignment)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Simple
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.Program
import Distribution.System (Platform)
import Distribution.Verbosity
import Hackage.Security.Util.Path (Path (..))
import Language.Nix
import Language.Nix.PrettyPrinting (Pretty (pPrint), braces, equals, fcat, hang, nest, parens, prettyShow, punctuate, semi, space, string, text, vcat, ($$), (<+>))
import NixXCabal.PackageSet (PackageEntry (..), PackageSet (..), PackageSource (..))
import NixXCabal.ReposConfig (ReposConfig (..), readReposConfig)
import NixXCabal.Repository (EditedCabal (..), PackageMetadata (PackageMetadata, cabalContents), RepositoryPackageMetadata, readPackageMetadata)

writePackagesNix :: FilePath -> FilePath -> Platform -> IO ()
writePackagesNix nixpkgsPath packageSetPath platform = do
  nixpkgs <- readNixpkgPackageMap nixpkgsPath (Just "{ config = { allowAliases = false; }; }")
  let nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs" :))) nixpkgs)
  packageSet :: PackageSet <- do
    contents <- BL.readFile packageSetPath
    either fail pure (eitherDecode contents)

  reposConfig' <- readReposConfig packageSet.reposConfig
  let repos = Map.assocs reposConfig'.repositories
  let entries = Map.toList packageSet.packages
      packageIds = [PackageIdentifier packageName entry.version | (packageName, Just entry) <- entries]
  metadata <-
    Map.fromList
      <$> traverse
        ( \(repository, config) -> do
            repositoryMetadata <- readPackageMetadata (repository, config) packageIds
            pure (repository, repositoryMetadata)
        )
        repos
  compiler <- do
    (compiler, _, _) <- GHC.configure normal (Just packageSet.ghc) (Just packageSet.ghcPkg) defaultProgramDb
    pure (compilerInfo compiler)
  entries <- mapM (writePackage nixpkgsResolver platform compiler metadata) entries
  putStr . prettyShow . PackagesNix $ entries
 where
  writePackage ::
    NixpkgsResolver ->
    Platform ->
    CompilerInfo ->
    Map.Map RepoName RepositoryPackageMetadata ->
    (PackageName, Maybe PackageEntry) ->
    IO (PackageName, Maybe Derivation)
  writePackage nixpkgsResolver targetPlatform targetCompiler metadata (packageName, mEntry) = do
    case mEntry of
      Nothing -> pure (packageName, Nothing)
      Just entry -> do
        let expectedPackageId = PackageIdentifier packageName entry.version

        PackageMetadata{cabalContents} <-
          maybe
            (fail ("missing .cabal file in repository index for " <> prettyShow expectedPackageId))
            pure
            (Map.lookup entry.repository metadata >>= Map.lookup packageName)
        let cabalContents' = BL.toStrict cabalContents
        genericDescription <- either (fail . show) pure (snd $ runParseResult $ parseGenericPackageDescription cabalContents')
        let actualPackageId = packageId genericDescription
        if actualPackageId /= expectedPackageId
          then fail ("package-set entry does not match cabal file for " <> unPackageName packageName)
          else pure ()
        let (source, editedCabal) = case entry.source of
              LocalSource{localPath = Path url} -> (localDerivationSource url, Nothing)
              RemoteSource{remoteUrl = Path url, remoteHash = hash, remoteEditedCabal = editedCabal'} ->
                (urlDerivationSource url hash, editedCabal')
            derivation =
              fromGenericPackageDescription
                (const True)
                nixpkgsResolver
                targetPlatform
                targetCompiler
                (mkFlagAssignment $ M.toList entry.flags)
                []
                genericDescription
                & testDepends .~ mempty
                & benchmarkDepends .~ mempty
                & doCheck .~ False
                & doBenchmark .~ False
                & jailbreak .~ entry.jailbreak
            derivationWithSource =
              derivation
                & src .~ source
                & maybe
                  id
                  ( \editedCabal ->
                      (revision .~ editedCabal.revision)
                        . (& editedCabalFile .~ editedCabal.hash)
                  )
                  editedCabal
         in pure (packageName, Just derivationWithSource)

  localDerivationSource url =
    DerivationSource
      { derivKind = Nothing
      , derivUrl = url
      , derivRevision = ""
      , derivHash = ""
      , derivSubmodule = Nothing
      , derivCustomSrc = Nothing
      }

newtype PackagesNix = PackagesNix [(PackageName, Maybe Derivation)]

instance Pretty PackagesNix where
  pPrint (PackagesNix packages) =
    text "{ pkgs, lib, callPackage }:"
      $$ text ""
      $$ text "self: {"
      $$ nest 2 (vcat (renderPackage <$> packages))
      $$ text "}"
   where
    renderPackage (unPackageName -> packageName, mDerivation) =
      (string packageName <+> equals)
        <+> maybe
          (text "null" <> semi)
          ( \derivation ->
              let overrides =
                    fcat
                      ( punctuate
                          space
                          [ pPrint b <> semi
                          | b <-
                              Set.toList
                                ( view (dependencies . each) derivation
                                    `Set.union` view extraFunctionArgs derivation
                                )
                          , not (isFromPackageSet b)
                          ]
                      )
                  package = hang (text "callPackage" <+> parens (pPrint derivation)) 2 (braces overrides <> semi)
               in package
          )
          mDerivation

isFromPackageSet :: Binding -> Bool
isFromPackageSet b = case b ^. (reference . path) of
  ["self", _] -> True
  _ -> False
