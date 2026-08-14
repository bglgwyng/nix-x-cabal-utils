{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as TarEntry
import Data.ByteString.Lazy qualified as BL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [state, input, output] -> cut state input output
    _ -> die "usage: cut-index-tar INDEX-STATE INPUT-TAR OUTPUT-TAR"

cut :: String -> FilePath -> FilePath -> IO ()
cut state input output = do
  cutoff <-
    maybe
      (die ("invalid ISO-8601 index-state: " <> state))
      pure
      (iso8601ParseM state :: Maybe UTCTime)
  entries <- Tar.read <$> BL.readFile input
  let kept = Tar.foldEntries (keep cutoff) [] (error . show) entries
  BL.writeFile output (Tar.write kept)
 where
  keep cutoff entry acc
    | entryTime entry <= cutoff = entry : acc
    | otherwise = acc

  entryTime entry =
    posixSecondsToUTCTime (fromIntegral (TarEntry.entryTime entry))
