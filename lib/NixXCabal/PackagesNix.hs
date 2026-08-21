{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module NixXCabal.PackagesNix (
  writePackagesNix,
)
where

import Control.Lens (each, over, review, view, (&), (.~), (^.))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Nixpkgs.Fetch (DerivationSource (..), urlDerivationSource)
import Distribution.Nixpkgs.Haskell.BuildInfo (haskell)
import Distribution.Nixpkgs.Haskell.Derivation (Derivation, benchmarkDepends, dependencies, doBenchmark, doCheck, editedCabalFile, extraFunctionArgs, jailbreak, revision, src, testDepends)
import Distribution.Nixpkgs.Haskell.Derivation qualified as Derivation
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
  let nixpkgsResolver = resolve (M.map (S.map (over path ("pkgs" :))) nixpkgs)
  packageSet :: PackageSet <- do
    contents <- BL.readFile packageSetPath
    either fail pure (eitherDecode contents)

  reposConfig' <- readReposConfig packageSet.reposConfig
  let repos = M.assocs reposConfig'.repositories
  let entries = packageSet.packages
      packageIds = map (.packageId) entries
  metadata <-
    M.fromList
      <$> traverse
        ( \(repository, config) -> do
            repositoryMetadata <- readPackageMetadata (repository, config) packageIds
            pure (repository, repositoryMetadata)
        )
        repos
  compiler <- do
    (compiler, _, _) <- GHC.configure normal (Just packageSet.ghc) (Just packageSet.ghcPkg) defaultProgramDb
    pure (compilerInfo compiler)
  versionedEntries <- mapM (writePackage nixpkgsResolver platform compiler metadata) entries
  let preExistingEntries =
        [ (versionedName (PackageIdentifier packageName version), Nothing)
        | (packageName, version) <- M.toList referencedVersions
        , PackageIdentifier packageName version `notElem` packageIds
        ]
      aliases =
        M.toList $
          M.union
            (M.fromList $ packageAliases packageSet.libraryVersions entries)
            ( M.fromList
                [ (packageName, Nothing)
                | entry <- entries
                , (packageName, Nothing) <- M.toList entry.setupDepends
                ]
            )
      referencedVersions =
        M.union
          packageSet.libraryVersions
          ( M.fromList
              [ (packageName, version)
              | entry <- entries
              , (packageName, Just version) <- M.toList entry.setupDepends
              ]
          )
  putStr . prettyShow . PackagesNix (versionedEntries <> preExistingEntries) $ aliases
 where
  writePackage ::
    NixpkgsResolver ->
    Platform ->
    CompilerInfo ->
    M.Map RepoName RepositoryPackageMetadata ->
    PackageEntry ->
    IO (String, Maybe Derivation)
  writePackage nixpkgsResolver targetPlatform targetCompiler metadata entry = do
    let expectedPackageId = entry.packageId
        packageName' = expectedPackageId.pkgName

    PackageMetadata{cabalContents} <-
      maybe
        (fail ("missing .cabal file in repository index for " <> prettyShow expectedPackageId))
        pure
        (M.lookup entry.repository metadata >>= M.lookup expectedPackageId)
    let cabalContents' = BL.toStrict cabalContents
    genericDescription <- either (fail . show) pure (snd $ runParseResult $ parseGenericPackageDescription cabalContents')
    let actualPackageId = packageId genericDescription
    if actualPackageId /= expectedPackageId
      then fail ("package-set entry does not match cabal file for " <> unPackageName packageName')
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
            & Derivation.setupDepends . haskell .~ S.fromList (setupBindings entry.setupDepends)
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
     in pure (versionedName entry.packageId, Just derivationWithSource)

  localDerivationSource url =
    DerivationSource
      { derivKind = Nothing
      , derivUrl = url
      , derivRevision = ""
      , derivHash = ""
      , derivSubmodule = Nothing
      , derivCustomSrc = Nothing
      }

  versionChar '.' = '_'
  versionChar '-' = '_'
  versionChar '+' = '_'
  versionChar c = c

  setupBindings setupDependencies =
    [ case version of
        Nothing ->
          review
            binding
            ( review ident (unPackageName name)
            , review path [review ident "self", review ident (unPackageName name)]
            )
        Just version' ->
          review
            binding
            ( review ident (versionedName (PackageIdentifier name version'))
            , review path [review ident "self", review ident (versionedName (PackageIdentifier name version'))]
            )
    | (name, version) <- M.toList setupDependencies
    ]

  packageAliases libraryVersions entries =
    M.toList
      (M.map aliasFor (M.fromListWith (<>) [(entry.packageId.pkgName, [entry]) | entry <- entries]))
      <> [ (packageName, Nothing)
         | (packageName, _) <- M.toList libraryVersions
         , not (any ((== packageName) . (.packageId.pkgName)) entries)
         ]
   where
    aliasFor packageEntries@(entry : _) =
      case M.lookup entry.packageId.pkgName libraryVersions of
        Just version ->
          maybe Nothing (Just . versionedName . (.packageId)) (find ((== version) . (.packageId.pkgVersion)) packageEntries)
        Nothing ->
          case packageEntries of
            [single] -> Just (versionedName single.packageId)
            _ -> Nothing
    aliasFor [] = Nothing

  versionedName :: PackageIdentifier -> String
  versionedName packageId' =
    unPackageName packageId'.pkgName <> "_" <> map versionChar (prettyShow packageId'.pkgVersion)

data PackagesNix = PackagesNix [(String, Maybe Derivation)] [(PackageName, Maybe String)]

instance Pretty PackagesNix where
  pPrint (PackagesNix packages aliases) =
    text "{ pkgs, lib, callPackage }:"
      $$ text ""
      $$ text "self: {"
      $$ nest 2 (vcat ((renderPackage <$> packages) <> (renderAlias <$> aliases)))
      $$ text "}"
   where
    renderPackage (packageName, mDerivation) =
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
                              S.toList
                                ( view (dependencies . each) derivation
                                    `S.union` view extraFunctionArgs derivation
                                )
                          , not (isFromPackageSet b)
                          ]
                      )
                  package = hang (text "callPackage" <+> parens (pPrint derivation)) 2 (braces overrides <> semi)
               in package
          )
          mDerivation

    renderAlias (packageName, Nothing) =
      string (unPackageName packageName)
        <+> equals
        <+> text "null;"
    renderAlias (packageName, Just versionedPackageName) =
      string (unPackageName packageName)
        <+> equals
        <+> (text "self." <> string versionedPackageName <> semi)

isFromPackageSet :: Binding -> Bool
isFromPackageSet b = case b ^. (reference . path) of
  ["self", _] -> True
  _ -> False
