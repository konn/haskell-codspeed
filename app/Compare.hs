{- | Compare two allocation sidecars and fail on regression.

@
codspeed-hs-compare baseline.csv current.csv [--tolerance 0.01]
@

Exits 1 if any benchmark allocates more than the tolerance allows, or has
disappeared from the suite. Intended as a CI step alongside CodSpeed rather than
instead of it: CodSpeed gates on instruction counts, this gates on a figure that
is exactly reproducible and available on hosts where simulation mode cannot run.
-}
module Main (main) where

import CodSpeed.Sidecar (
  Comparison (..),
  Record,
  Verdict (..),
  compareRuns,
  parseCsv,
  regressions,
  renderComparison,
 )
import Control.Monad (forM_, unless)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

defaultTolerance :: Double
defaultTolerance = 0.01

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> usage >> exitFailure
    Just (basePath, currPath, tolerance) -> do
      base <- readSidecar basePath
      curr <- readSidecar currPath
      let cmps = compareRuns tolerance base curr
          bad = regressions cmps

      forM_ (filter (interesting . cmpVerdict) cmps) $ \c ->
        putStrLn ("  " <> renderComparison c)

      let unchanged = length cmps - length (filter (interesting . cmpVerdict) cmps)
      unless (unchanged == 0) $
        putStrLn ("  " <> show unchanged <> " unchanged")

      if null bad
        then do
          putStrLn "allocation: no regressions"
          exitSuccess
        else do
          hPutStrLn stderr $
            "allocation: "
              <> show (length bad)
              <> " regression(s) beyond "
              <> show (tolerance * 100)
              <> "%"
          exitFailure
  where
    interesting Unchanged = False
    interesting _ = True

readSidecar :: FilePath -> IO [Record]
readSidecar path = do
  contents <- readFile path
  case parseCsv contents of
    Left err -> do
      hPutStrLn stderr (path <> ": " <> err)
      exitFailure
    Right rs -> pure rs

usage :: IO ()
usage = do
  me <- getProgName
  hPutStrLn stderr $
    unlines
      [ "usage: " <> me <> " BASELINE.csv CURRENT.csv [--tolerance FRACTION]"
      , ""
      , "  Compares per-benchmark allocation between two sidecar files written by"
      , "  Test.Tasty.Bench.CodSpeed. Exits non-zero on regression."
      , ""
      , "  --tolerance  fractional change treated as noise (default "
          <> show defaultTolerance
          <> ")"
      ]

parseArgs :: [String] -> Maybe (FilePath, FilePath, Double)
parseArgs (b : c : rest) = (,,) b c <$> tolerance rest
  where
    tolerance [] = Just defaultTolerance
    tolerance ["--tolerance", t] = case reads t of
      [(v, "")] | v >= 0 -> Just v
      _ -> Nothing
    tolerance _ = Nothing
parseArgs _ = Nothing
