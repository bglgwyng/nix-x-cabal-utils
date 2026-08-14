{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.NixOutput (
  writeNixExpressions,
)
where

import Control.Lens ((&), (.~))
import Data.Aeson (decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Nixpkgs.Fetch (urlDerivationSource)
import Distribution.Nixpkgs.Haskell.Derivation (Derivation, editedCabalFile, revision, src)
import Distribution.Nixpkgs.Haskell.FromCabal (fromGenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Simple
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.Program
import Distribution.System (Platform)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Verbosity
import Language.Nix.Binding (Binding)
import Language.Nix.Identifier (Identifier)
import Language.Nix.PrettyPrinting (Pretty (pPrint), equals, nest, prettyShow, string, text, vcat, ($$), (<+>))
import NixXCabal.PackageSet (PackageEntry (..), PackageSet (..))
import NixXCabal.ReposConfig (RepositoryConfig (..), readReposConfig, repositories, singleRepository)
import NixXCabal.Repository (readPackageMetadata)

writeNixExpressions :: FilePath -> Platform -> IO ()
writeNixExpressions packageSetPath platform = do
  packageSet <- do
    contents <- BL.readFile packageSetPath
    maybe (fail ("invalid package-set manifest: " <> packageSetPath)) pure (decode contents)

  reposConfig' <- readReposConfig (reposConfig packageSet)
  repo@(repoName, _) <- either fail pure (singleRepository (repositories reposConfig'))
  let entries = Map.toList (packages packageSet)
      packageIds = [entryPackageId packageName entry | (packageName, entry) <- entries]
  metadata <- readPackageMetadata repo packageIds
  compiler <- do
    (compiler, _, _) <- GHC.configure normal (Just packageSet.ghc) (Just packageSet.ghcPkg) defaultProgramDb
    pure (compilerInfo compiler)
  entries <- mapM (writePackage platform compiler repoName metadata) entries
  putStr (renderPackageSet (map packageDerivation entries))
 where
  writePackage :: Platform -> CompilerInfo -> RepoName -> Map.Map (RepoName, PackageId) (Maybe String, Maybe (Int, String), BL.ByteString) -> (String, PackageEntry) -> IO (String, PackageId, Derivation)
  writePackage targetPlatform targetCompiler repository metadata (packageName, entry) = do
    let expectedPackageId = entryPackageId packageName entry
    (_, _, cabalContents) <- maybe (fail ("missing .cabal file in repository index for " <> prettyShow expectedPackageId)) pure (Map.lookup (repository, expectedPackageId) metadata)
    let cabalContents' = BL.toStrict cabalContents
    genericDescription <- parseCabal (packageName <> ".cabal") cabalContents'
    let actualPackageId = packageId genericDescription
    if actualPackageId /= expectedPackageId
      then fail ("package-set entry does not match cabal file for " <> packageName)
      else pure ()
    hashValue <- maybe (fail ("missing source hash for " <> packageName)) pure (hash entry)
    let derivation =
          fromGenericPackageDescription
            (const True)
            nixpkgsResolver
            targetPlatform
            targetCompiler
            mempty
            []
            genericDescription
        derivationWithSource =
          setRevision
            (entry.packageRevision)
            (entry.editedCabalHash)
            (setSource (entry.url) hashValue derivation)
    pure (packageName, packageId genericDescription, derivationWithSource)

  packageDerivation :: (String, PackageId, Derivation) -> (String, Derivation)
  packageDerivation (packageName, _, derivation) = (packageName, derivation)

  parseCabal :: FilePath -> BS.ByteString -> IO GenericPackageDescription
  parseCabal cabalPath contents =
    case runParseResult (parseGenericPackageDescription contents) of
      (_, Right description) -> pure description
      (_, Left (_, errors)) -> fail ("invalid cabal file " <> cabalPath <> ": " <> show errors)

  setSource :: String -> String -> Derivation -> Derivation
  setSource sourceUrl sourceHash derivation =
    derivation & src .~ urlDerivationSource sourceUrl sourceHash

  setRevision packageRevision' editedCabalHash' derivation =
    derivation
      & revision .~ maybe 0 id packageRevision'
      & editedCabalFile .~ maybe "" id editedCabalHash'

  nixpkgsResolver :: Identifier -> Maybe Binding
  nixpkgsResolver _ = Nothing

  entryPackageId :: String -> PackageEntry -> PackageId
  entryPackageId packageName entry = fromMaybeParse (packageName <> "-" <> prettyShow (version entry))

  fromMaybeParse :: String -> PackageId
  fromMaybeParse value =
    case simpleParsec value of
      Just packageId' -> packageId'
      Nothing -> error ("invalid package id in package-set manifest: " <> value)

newtype PackageSetExpression = PackageSetExpression [(String, Derivation)]

instance Pretty PackageSetExpression where
  pPrint (PackageSetExpression packages) =
    text "{ pkgs, lib, callPackage }:"
      $$ text ""
      $$ text "self: {"
      $$ nest 2 (vcat (map renderPackage packages))
      $$ text "};"
   where
    renderPackage (packageName, derivation) =
      string packageName
        <+> equals
        <+> text "callPackage"
        <+> text "("
        $$ nest 2 (pPrint derivation)
        $$ text ") {};"

renderPackageSet :: [(String, Derivation)] -> String
renderPackageSet = prettyShow . PackageSetExpression
