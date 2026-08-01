{- | Per-benchmark allocation, recorded alongside CodSpeed's own measurement.

CodSpeed tracks one number per benchmark and compares it against a server-side
baseline. There is no channel for a second metric, so GHC's allocation figures —
the ones that are exactly deterministic — have nowhere to go inside CodSpeed.

This module gives them somewhere: a CSV artifact the run writes itself, which can
be committed as a baseline, diffed in a pull request, and gated on independently
of whether CodSpeed ran at all. That last part matters, because allocation is
measurable on macOS and Windows where the CPU-simulation instrument cannot run.

A copy also goes into @$CODSPEED_PROFILE_FOLDER@ when it is set. The runner tars
that folder wholesale and uploads it without inspecting the contents, so the data
is preserved at no cost should CodSpeed ever grow a custom-metric channel.

== Why CSV

It parses without a dependency, diffs legibly in review, and matches what
@tasty-bench@'s own @--csv@ and @--baseline@ already speak. Allocation is an
integer count of bytes, so there is nothing here that needs a richer format.
-}
module CodSpeed.Sidecar (
  -- * Records
  Record (..),
  fromMeasurement,

  -- * Serialising
  renderCsv,
  parseCsv,
  writeSidecar,

  -- * Comparing
  Comparison (..),
  Verdict (..),
  compareRuns,
  regressions,
  renderComparison,
) where

import CodSpeed.Instrument.Raw (c_getpid)
import CodSpeed.RTS.Stats (
  GCDelta (..),
  Measurement (..),
 )
import Data.List (sortOn)
import Data.Word (Word32, Word64)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | One benchmark's deterministic figures.
data Record = Record
  { recUri :: !String
  -- ^ The same URI reported to CodSpeed, so the two can be joined.
  , recAllocatedBytes :: !Word64
  -- ^ Exactly reproducible for fixed code and input. This is the gated number.
  , recCopiedBytes :: !Word64
  {- ^ Recorded for diagnosis, never gated.

  Unlike allocation this is /not/ reproducible: it depends on what the
  collector chose to do. Two runs of an identical binary here reported
  9591818 bytes allocated both times, and 580680 vs 580663 bytes copied.
  -}
  , recCollections :: !Word32
  {- ^ Collections during the measured window. Not gated by default: any
  benchmark allocating more than @-A@ will legitimately have some.
  -}
  , recMajorCollections :: !Word32
  }
  deriving (Show, Eq)

-- | Build a record from what "CodSpeed.RTS.Stats" measured.
fromMeasurement :: String -> Measurement -> Record
fromMeasurement uri m =
  Record
    { recUri = uri
    , recAllocatedBytes = measAllocatedBytes m
    , recCopiedBytes = maybe 0 gcCopiedBytes (measGC m)
    , recCollections = maybe 0 gcCollections (measGC m)
    , recMajorCollections = maybe 0 gcMajorCollections (measGC m)
    }

header :: String
header = "uri,allocated_bytes,copied_bytes,collections,major_collections"

{- | Render records as CSV, sorted by URI so the output is stable enough to commit
and diff.

>>> putStr (renderCsv [Record "a::b" 100 8 1 0])
uri,allocated_bytes,copied_bytes,collections,major_collections
a::b,100,8,1,0
-}
renderCsv :: [Record] -> String
renderCsv rs = unlines (header : map row (sortOn recUri rs))
  where
    row r =
      quote (recUri r)
        <> ","
        <> show (recAllocatedBytes r)
        <> ","
        <> show (recCopiedBytes r)
        <> ","
        <> show (recCollections r)
        <> ","
        <> show (recMajorCollections r)

{- | Quote a field only when it needs it, so ordinary URIs stay readable in a
diff.
-}
quote :: String -> String
quote s
  | any (`elem` (",\"\r\n" :: String)) s = '"' : concatMap escape s <> "\""
  | otherwise = s
  where
    escape '"' = "\"\""
    escape c = [c]

{- | Parse what 'renderCsv' produced.

Returns 'Left' with a line number on malformed input rather than throwing, since
this reads files that may have been hand-edited or produced by an older version.

>>> parseCsv "uri,allocated_bytes,copied_bytes,collections,major_collections\na::b,100,8,1,0\n"
Right [Record {recUri = "a::b", recAllocatedBytes = 100, recCopiedBytes = 8, recCollections = 1, recMajorCollections = 0}]

>>> parseCsv "uri,allocated_bytes,copied_bytes,collections,major_collections\nbad\n"
Left "line 2: expected 5 fields, got 1"
-}
parseCsv :: String -> Either String [Record]
parseCsv input = case lines (filter (/= '\r') input) of
  [] -> Right []
  (h : rest)
    | h /= header -> Left ("unexpected header: " <> h)
    | otherwise -> traverse parseRow (zip [2 :: Int ..] (filter (not . null) rest))
  where
    parseRow (lineNo, l) = case splitFields l of
      [u, a, c, g, mg] ->
        Record u
          <$> num lineNo "allocated_bytes" a
          <*> num lineNo "copied_bytes" c
          <*> num lineNo "collections" g
          <*> num lineNo "major_collections" mg
      fs -> Left ("line " <> show lineNo <> ": expected 5 fields, got " <> show (length fs))

    num :: (Read a) => Int -> String -> String -> Either String a
    num lineNo field s = case reads s of
      [(v, "")] -> Right v
      _ -> Left ("line " <> show lineNo <> ": bad " <> field <> ": " <> s)

-- | Split a CSV line, honouring the quoting 'quote' applies.
splitFields :: String -> [String]
splitFields = go
  where
    go s = case s of
      '"' : rest -> quoted rest ""
      _ -> plain s ""

    plain [] acc = [reverse acc]
    plain (',' : rest) acc = reverse acc : go rest
    plain (c : rest) acc = plain rest (c : acc)

    quoted ('"' : '"' : rest) acc = quoted rest ('"' : acc)
    quoted ('"' : ',' : rest) acc = reverse acc : go rest
    quoted ('"' : _) acc = [reverse acc]
    quoted (c : rest) acc = quoted rest (c : acc)
    quoted [] acc = [reverse acc]

{- | Write the artifact to @path@ when one is given, and always drop a copy into
@$CODSPEED_PROFILE_FOLDER@ when the runner has set one.

The profile-folder copy is unconditional because it is free: the runner tars that
directory blind and uploads it, so the data rides along whether or not anyone has
asked for a local file.
-}
writeSidecar :: Maybe FilePath -> [Record] -> IO ()
writeSidecar path rs = do
  let body = renderCsv rs
  case path of
    Just p -> writeFile p body
    Nothing -> pure ()
  folder <- lookupEnv "CODSPEED_PROFILE_FOLDER"
  case folder of
    Just dir | not (null dir) -> do
      pid <- c_getpid
      createDirectoryIfMissing True dir
      writeFile (dir </> ("haskell-rts-" <> show pid <> ".csv")) body
    _ -> pure ()

-- | What happened to one benchmark between two runs.
data Verdict
  = Unchanged
  | -- | Fractional change, negative.
    Improved !Double
  | -- | Fractional change, positive.
    Regressed !Double
  | Added
  | Removed
  deriving (Show, Eq)

data Comparison = Comparison
  { cmpUri :: !String
  , cmpBaseline :: !(Maybe Word64)
  , cmpCurrent :: !(Maybe Word64)
  , cmpVerdict :: !Verdict
  }
  deriving (Show, Eq)

{- | Compare two runs' allocation, at a fractional tolerance.

A tolerance of @0.01@ treats anything within 1% as unchanged. Zero would be
defensible — allocation really is exact — but in practice a benchmark's input or
a library's internals shift by a hair between commits, and a gate that fires on
every such change gets switched off.

Benchmarks present in only one run are reported as 'Added' or 'Removed' rather
than silently ignored: a benchmark disappearing from a suite is exactly the kind
of thing a gate should notice.

>>> map cmpVerdict (compareRuns 0.01 [Record "a" 1000 0 0 0] [Record "a" 1200 0 0 0])
[Regressed 0.2]

>>> map cmpVerdict (compareRuns 0.01 [Record "a" 1000 0 0 0] [Record "a" 1005 0 0 0])
[Unchanged]
-}
compareRuns :: Double -> [Record] -> [Record] -> [Comparison]
compareRuns tolerance baseline current =
  sortOn cmpUri (map judge (allUris baseline current))
  where
    judge uri = case (lookupAlloc uri baseline, lookupAlloc uri current) of
      (Nothing, Nothing) -> Comparison uri Nothing Nothing Unchanged
      (Nothing, Just c) -> Comparison uri Nothing (Just c) Added
      (Just b, Nothing) -> Comparison uri (Just b) Nothing Removed
      (Just b, Just c) -> Comparison uri (Just b) (Just c) (verdictFor b c)

    verdictFor b c
      | b == 0 && c == 0 = Unchanged
      -- Growing from nothing has no meaningful ratio, but it is still a change
      -- worth surfacing.
      | b == 0 = Regressed 1
      | abs delta <= tolerance = Unchanged
      | delta > 0 = Regressed delta
      | otherwise = Improved delta
      where
        delta = (fromIntegral c - fromIntegral b) / fromIntegral b :: Double

    lookupAlloc uri = fmap recAllocatedBytes . lookup uri . map (\r -> (recUri r, r))

allUris :: [Record] -> [Record] -> [String]
allUris a b = foldr insertUnique [] (map recUri a <> map recUri b)
  where
    insertUnique x xs
      | x `elem` xs = xs
      | otherwise = x : xs

-- | The comparisons a CI gate should fail on.
regressions :: [Comparison] -> [Comparison]
regressions = filter (isRegression . cmpVerdict)
  where
    isRegression (Regressed _) = True
    isRegression Removed = True
    isRegression _ = False

-- | A human-readable summary line per changed benchmark.
renderComparison :: Comparison -> String
renderComparison c = case cmpVerdict c of
  Unchanged -> cmpUri c <> ": unchanged"
  Added -> cmpUri c <> ": new (" <> bytes (cmpCurrent c) <> ")"
  Removed -> cmpUri c <> ": REMOVED (was " <> bytes (cmpBaseline c) <> ")"
  Improved d -> cmpUri c <> ": " <> pct d <> " " <> transition c
  Regressed d -> cmpUri c <> ": " <> pct d <> " " <> transition c
  where
    bytes = maybe "-" (\n -> show n <> " B")
    transition x = bytes (cmpBaseline x) <> " -> " <> bytes (cmpCurrent x)
    pct d =
      let scaled = fromIntegral (round (d * 1000) :: Int) / 10 :: Double
       in (if d > 0 then "+" else "") <> show scaled <> "%"
