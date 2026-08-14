{-# LANGUAGE OverloadedStrings #-}

module NixXCabal.PackagesConfig (
  PackagesConfig (..),
  ResolutionConfig (..),
  readPackagesConfig,
)
where

import Data.Aeson (FromJSON (..), decode, withObject, (.!=), (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.String (fromString)
import Distribution.Client.Types.AllowNewer (
  AllowNewer (..),
  AllowOlder (..),
  RelaxDepMod (..),
  RelaxDepScope (..),
  RelaxDepSubject (..),
  RelaxDeps (..),
  RelaxedDep (..),
 )
import Distribution.Package (PackageName, pkgVersion)
import Distribution.Parsec (simpleParsec)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (ConstraintSourceUnknown))
import Distribution.Solver.Types.LabeledPackageConstraint (LabeledPackageConstraint (..))
import Distribution.Solver.Types.PackageConstraint (
  PackageConstraint (..),
  PackageProperty (PackagePropertyVersion),
  scopeToplevel,
 )
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Version (versionNumbers)

data PackagesConfig = PackagesConfig
  { packages :: [PackageName]
  , resolution :: ResolutionConfig
  }

instance FromJSON PackagesConfig where
  parseJSON = withObject "PackagesConfig" $ \o ->
    PackagesConfig
      <$> (map fromString <$> (o .:? "packages" .!= []))
      <*> (o .:? "resolution" .!= emptyResolution)

data ResolutionConfig = ResolutionConfig
  { allowNewer :: AllowNewer
  , allowOlder :: AllowOlder
  , constraints :: [LabeledPackageConstraint]
  }

instance FromJSON ResolutionConfig where
  parseJSON = withObject "ResolutionConfig" $ \o -> do
    newerValues <- o .:? "allow-newer" .!= []
    olderValues <- o .:? "allow-older" .!= []
    newer <- either fail (pure . AllowNewer) (parseRelaxDeps newerValues)
    older <- either fail (pure . AllowOlder) (parseRelaxDeps olderValues)
    rawConstraints <- o .:? "constraints" .!= []
    parsed <- traverse parseConstraint rawConstraints
    pure $
      ResolutionConfig
        newer
        older
        [ LabeledPackageConstraint
            (PackageConstraint (scopeToplevel name) (PackagePropertyVersion versionRange))
            ConstraintSourceUnknown
        | PackageVersionConstraint name versionRange <- parsed
        ]
   where
    parseConstraint value =
      maybe (fail ("invalid constraint: " <> value)) pure (simpleParsec value)

emptyResolution :: ResolutionConfig
emptyResolution = ResolutionConfig (AllowNewer (RelaxDepsSome [])) (AllowOlder (RelaxDepsSome [])) []

parseRelaxDeps :: [String] -> Either String RelaxDeps
parseRelaxDeps values = RelaxDepsSome <$> traverse parseRelaxedDep values

parseRelaxedDep :: String -> Either String RelaxedDep
parseRelaxedDep value =
  let (scopeText, subjectText) = case break (== ':') value of
        (scope, ':' : subject) -> (scope, subject)
        (subject, _) -> ("*", subject)
      (modifier, subjectName) = case subjectText of
        '^' : rest -> (RelaxDepModCaret, rest)
        rest -> (RelaxDepModNone, rest)
   in RelaxedDep <$> parseScope scopeText <*> pure modifier <*> parseSubject subjectName
 where
  parseScope "*" = Right RelaxDepScopeAll
  parseScope "all" = Right RelaxDepScopeAll
  parseScope scope =
    case simpleParsec scope of
      Just packageId
        | not (null (versionNumbers (pkgVersion packageId))) ->
            Right (RelaxDepScopePackageId packageId)
      _ -> Right (RelaxDepScopePackage (fromString scope))
  parseSubject "*" = Right RelaxDepSubjectAll
  parseSubject "all" = Right RelaxDepSubjectAll
  parseSubject subject = Right (RelaxDepSubjectPkg (fromString subject))

readPackagesConfig :: FilePath -> IO PackagesConfig
readPackagesConfig path = do
  contents <- BL.readFile path
  maybe (fail ("invalid packages config: " <> path)) pure (decode contents)
