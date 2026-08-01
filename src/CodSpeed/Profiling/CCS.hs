{- | Reading GHC's cost-centre stack tree at runtime.

With @-fprof-late@ GHC inserts cost centres after optimisation, so the tree
describes the program that actually runs rather than the one that was written.
This module walks that tree, which is the raw material for a Haskell-shaped
flamegraph.

== Why this exists

CodSpeed's flamegraphs come from Callgrind, whose frames are ELF symbols. A cost
centre is not a symbol — @-fprof-late@ emits inline @pushCostCentre@ and static
data, and no code label — so no combination of GHC or Valgrind flags can make one
appear as a Callgrind frame. The only way cost-centre structure reaches a
flamegraph is if something reads it here and writes the profile itself.

That leaves two consumers, and both need this module:

* rewriting the callgrind profile so its call graph is cost-centre shaped, and
* emitting a standalone profile ('foldedStacks') for @speedscope@ or
  @flamegraph.pl@, which needs nothing from CodSpeed at all.

== Requires a profiling build

There is no cost-centre tree without @-prof@. 'ccsAvailable' reports whether this
binary has one; everything else returns 'Nothing' or an empty tree otherwise. So a
suite can call into this unconditionally and simply get nothing back from a
vanilla build.

== Scoping to one benchmark

The RTS offers no way to reset cost-centre counters — there is no @resetCCS@ in
@rts\/prof\/CCS.h@ — so per-benchmark figures come from taking a snapshot before
and after and subtracting ('diffCCS'). Cost-centre stacks live in a dedicated
arena and are never freed during a run, so a node observed in the first snapshot
is still valid at the second.

== What is deliberately not counted

Only @CCS_MAIN@ is walked. @CCS_GC@, @CCS_SYSTEM@ and @CCS_OVERHEAD@ are separate
roots holding runtime costs; folding them in would attribute collector work to
whichever Haskell function happened to be running.

Entries and allocation are counter-driven and exact. Time ticks are not read at
all: they come from the RTS sampling timer, which under a ~200x Valgrind slowdown
measures the simulator rather than the program.
-}
module CodSpeed.Profiling.CCS (
  -- * The tree
  CCSNode (..),
  ccsAvailable,
  snapshotCCS,

  -- * Scoping
  diffCCS,
  currentCCSId,
  findById,

  -- * Output
  Weight (..),
  foldedStacks,
  stripCommonPrefix,
  pruneSelfProfiling,
  qualified,
  writeFoldedStacks,
  uriToFileName,

  -- * Summarising
  totalEntries,
  totalAllocBytes,
  flatten,
) where

import Control.Monad (unless)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Word (Word32, Word64)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import GHC.Stack.CCS (CostCentre, ccLabel, ccModule, ccSrcSpan, getCurrentCCS)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

-- | One node of the cost-centre stack tree.
data CCSNode = CCSNode
  { ccsId :: !Int64
  {- ^ The RTS's identifier for this stack. Stable within a run, and what
  'diffCCS' matches on.
  -}
  , ccsLabel :: !String
  {- ^ For @-fprof-late@ this is the post-optimisation top-level binder's
  @OccName@ — so it lines up with the name in a GHC symbol, modulo
  z-encoding and worker-wrapper @$w@ prefixes.
  -}
  , ccsModule :: !String
  , ccsSrcSpan :: !String
  , ccsEntries :: !Word64
  -- ^ Times this stack was entered. Exact and reproducible.
  , ccsAllocBytes :: !Word64
  {- ^ Bytes allocated by this stack, exclusive of children. Exact.

  The RTS counts words; the conversion happens here.
  -}
  , ccsChildren :: [CCSNode]
  }
  deriving (Show, Eq)

foreign import ccall unsafe "hs_codspeed_ccs_available"
  c_available :: IO CInt

foreign import ccall unsafe "hs_codspeed_ccs_word_size"
  c_wordSize :: IO Word32

foreign import ccall unsafe "hs_codspeed_ccs_root"
  c_root :: IO (Ptr ())

foreign import ccall unsafe "hs_codspeed_ccs_id"
  c_ccsId :: Ptr () -> IO Int64

foreign import ccall unsafe "hs_codspeed_ccs_cc"
  c_ccsCC :: Ptr () -> IO (Ptr CostCentre)

foreign import ccall unsafe "hs_codspeed_ccs_entries"
  c_ccsEntries :: Ptr () -> IO Word64

foreign import ccall unsafe "hs_codspeed_ccs_alloc_words"
  c_ccsAllocWords :: Ptr () -> IO Word64

foreign import ccall unsafe "hs_codspeed_ccs_children"
  c_ccsChildren :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "hs_codspeed_it_next"
  c_itNext :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "hs_codspeed_it_ccs"
  c_itCCS :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "hs_codspeed_it_back_edge"
  c_itBackEdge :: Ptr () -> IO CInt

{- | Whether this binary was built the profiling way.

A constant for the lifetime of the process, so it is safe to read once.
-}
ccsAvailable :: Bool
ccsAvailable = unsafePerformIO ((/= 0) <$> c_available)
{-# NOINLINE ccsAvailable #-}

wordSize :: Word64
wordSize = unsafePerformIO (fromIntegral <$> c_wordSize)
{-# NOINLINE wordSize #-}

{- | Capture the cost-centre tree as it stands.

'Nothing' from a vanilla build. Cost is proportional to the number of distinct
cost-centre stacks, which grows with the program's call structure rather than
with how long it ran.
-}
snapshotCCS :: IO (Maybe CCSNode)
snapshotCCS
  | not ccsAvailable = pure Nothing
  | otherwise = do
      root <- c_root
      if root == nullPtr then pure Nothing else Just <$> readNode root

readNode :: Ptr () -> IO CCSNode
readNode ccs = do
  nodeId <- c_ccsId ccs
  cc <- c_ccsCC ccs
  label <- peekCC ccLabel cc
  modName <- peekCC ccModule cc
  src <- peekCC ccSrcSpan cc
  entries <- c_ccsEntries ccs
  allocWords <- c_ccsAllocWords ccs
  children <- readChildren =<< c_ccsChildren ccs
  pure
    CCSNode
      { ccsId = nodeId
      , ccsLabel = label
      , ccsModule = modName
      , ccsSrcSpan = src
      , ccsEntries = entries
      , ccsAllocBytes = allocWords * wordSize
      , ccsChildren = children
      }
  where
    peekCC f p
      | p == nullPtr = pure ""
      | otherwise = peekCString =<< f p

{- | Walk an 'IndexTable' linked list.

Back edges are skipped: they re-enter a cost centre already on the stack, so
following them would not terminate.
-}
readChildren :: Ptr () -> IO [CCSNode]
readChildren it
  | it == nullPtr = pure []
  | otherwise = do
      back <- c_itBackEdge it
      rest <- readChildren =<< c_itNext it
      if back /= 0
        then pure rest
        else do
          child <- c_itCCS it
          if child == nullPtr
            then pure rest
            else (: rest) <$> readNode child

{- | Subtract an earlier snapshot from a later one, giving what happened in
between.

Nodes are matched on 'ccsId'. A node present only in the later snapshot is kept
whole — it came into existence during the window, so all of its cost belongs to
it. Counters are monotonic, so the subtraction is saturating only as defence
against a mismatched pair being passed in.
-}
diffCCS :: CCSNode -> CCSNode -> CCSNode
diffCCS before after =
  after
    { ccsEntries = ccsEntries after `sub` ccsEntries before
    , ccsAllocBytes = ccsAllocBytes after `sub` ccsAllocBytes before
    , ccsChildren = map diffChild (ccsChildren after)
    }
  where
    priorById = Map.fromList [(ccsId c, c) | c <- ccsChildren before]
    diffChild c = case Map.lookup (ccsId c) priorById of
      Just prior -> diffCCS prior c
      Nothing -> c
    sub a b = if a >= b then a - b else 0

{- | The identifier of the cost-centre stack the caller is currently running on.

Call this at the point a benchmark begins and hand the result to 'findById' to
re-root the profile there. Without it the tree is rooted at @MAIN@ and every path
carries a dozen-odd frames of @main@, tasty's scheduler and the harness before
reaching any code under test — which is most of the width of the flamegraph and
all of it noise.

Trimming the shared prefix instead does not work, because CAFs and the RTS's own
pseudo-roots (@SYSTEM@, @PROFILING.OVERHEAD_of@) hang directly off @MAIN@, so
there is no shared prefix beyond @MAIN@ itself.

'Nothing' in a vanilla build.
-}
currentCCSId :: IO (Maybe Int64)
currentCCSId
  | not ccsAvailable = pure Nothing
  | otherwise = do
      ccs <- getCurrentCCS ()
      let p = castPtr ccs
      if p == nullPtr then pure Nothing else Just <$> c_ccsId p

{- | Find the subtree rooted at a given stack.

>>> let t = CCSNode 1 "a" "M" "" 0 0 [CCSNode 2 "b" "M" "" 0 0 []]
>>> fmap ccsLabel (findById 2 t)
Just "b"
>>> fmap ccsLabel (findById 99 t)
Nothing
-}
findById :: Int64 -> CCSNode -> Maybe CCSNode
findById target node
  | ccsId node == target = Just node
  | otherwise = firstJust (map (findById target) (ccsChildren node))
  where
    firstJust (Just x : _) = Just x
    firstJust (Nothing : rest) = firstJust rest
    firstJust [] = Nothing

{- | Drop subtrees belonging to this module's own tree walk.

Self-measurement has an irreducible edge: taking the closing snapshot allocates,
that allocation is recorded against whatever cost-centre stack is current, and it
therefore appears in the difference. Left in it is conspicuous — a benchmark that
allocates nothing at all was reported as spending 7.5 MB in @$wreadNode@ and the
'peekCString' calls that decode cost-centre labels.

Pruning is honest here in a way that adjusting the numbers would not be: these
frames are this library, not the program under test, and they could not have been
present had the profile not been taken.

>>> let t = CCSNode 1 "a" "M" "" 0 0 [CCSNode 2 "r" "CodSpeed.Profiling.CCS" "" 0 99 []]
>>> map ccsLabel (ccsChildren (pruneSelfProfiling t))
[]
-}
pruneSelfProfiling :: CCSNode -> CCSNode
pruneSelfProfiling node =
  node {ccsChildren = map pruneSelfProfiling (filter (not . isSelf) (ccsChildren node))}
  where
    isSelf n = ccsModule n == "CodSpeed.Profiling.CCS"

-- | Which figure to weight a flamegraph by.
data Weight
  = -- | Bytes allocated. Exact, and usually the more informative for Haskell.
    ByAllocation
  | -- | Times entered. Exact, and better for spotting unexpectedly hot paths.
    ByEntries
  deriving (Show, Eq)

{- | Render as folded stacks: one line per path, @a;b;c \<count\>@.

This is what @flamegraph.pl@ and @speedscope@ read, so it is the cheapest useful
thing to emit — no CodSpeed involvement required.

Frames are @Module.label@. Zero-weight paths are dropped, since they only pad the
file. Output is ordered heaviest first so the head of the file is the interesting
part.

Frames every path shares are dropped first — see 'stripCommonPrefix'.

>>> let leaf = CCSNode 2 "go" "M" "" 3 64 []
>>> let root = CCSNode 1 "MAIN" "MAIN" "" 1 16 [leaf]
>>> mapM_ putStrLn (foldedStacks ByAllocation root)
MAIN.MAIN;M.go 64
MAIN.MAIN 16
-}
foldedStacks :: Weight -> CCSNode -> [String]
foldedStacks weight =
  map render . sortOn (Down . snd) . stripCommonPrefix . collect []
  where
    collect prefix node =
      let path = prefix <> [qualified node]
          self = weightOf node
          here = [(path, self) | self > 0]
       in here <> concatMap (collect path) (ccsChildren node)

    weightOf n = case weight of
      ByAllocation -> ccsAllocBytes n
      ByEntries -> ccsEntries n

    render (path, n) = intercalateSemi path <> " " <> show n
    intercalateSemi = foldr1 (\a b -> a <> ";" <> b)

{- | Drop the frames every path shares, keeping the last of them as the new root.

A per-benchmark profile is captured from inside the harness, so every path starts
with the same dozen-odd frames of @main@, tasty's scheduler and this package's own
runner before reaching any of the code under test. Left in, they are most of the
width of the flamegraph and all of it is noise.

Dropping them re-roots the graph at the point where the paths actually diverge.
The last shared frame is kept so the result still has a single root.

>>> stripCommonPrefix [(["a","b","c"], 1), (["a","b","d"], 2)]
[(["b","c"],1),(["b","d"],2)]

A single path has nothing to diverge at, so it is left alone:

>>> stripCommonPrefix [(["a","b"], 1)]
[(["a","b"],1)]
-}
stripCommonPrefix :: [([String], Word64)] -> [([String], Word64)]
stripCommonPrefix [] = []
stripCommonPrefix [p] = [p]
stripCommonPrefix paths = map (\(p, n) -> (drop toDrop p, n)) paths
  where
    shared = foldr1 commonPrefix (map fst paths)
    -- Keep the last shared frame as the root.
    toDrop = max 0 (length shared - 1)
    commonPrefix (a : as) (b : bs) | a == b = a : commonPrefix as bs
    commonPrefix _ _ = []

{- | Write folded stacks for one benchmark into @dir@, named after its URI.

The file is what @speedscope@ and @flamegraph.pl@ consume directly:

@
speedscope ccs\/bench_Example.hs__sort__1000.folded
@

Writes nothing when the tree is empty, so a vanilla build leaves no litter.
-}
writeFoldedStacks :: FilePath -> Weight -> String -> CCSNode -> IO ()
writeFoldedStacks dir weight uri node = do
  let body = foldedStacks weight (pruneSelfProfiling node)
  unless (null body) $ do
    createDirectoryIfMissing True dir
    writeFile (dir </> uriToFileName uri <> ".folded") (unlines body)

{- | Turn a benchmark URI into something safe to use as a filename.

>>> uriToFileName "example:All.fib.1000.leaky"
"example_All.fib.1000.leaky"
-}
uriToFileName :: String -> FilePath
uriToFileName = go
  where
    go (':' : ':' : rest) = '_' : '_' : go rest
    go (c : rest)
      | c `elem` ("/\\:*?\"<>| " :: String) = '_' : go rest
      | otherwise = c : go rest
    go [] = []

{- | @Module.label@, the frame name used in output.

>>> qualified (CCSNode 1 "solve" "Logic.SAT" "" 0 0 [])
"Logic.SAT.solve"
-}
qualified :: CCSNode -> String
qualified n
  | null (ccsModule n) = ccsLabel n
  | otherwise = ccsModule n <> "." <> ccsLabel n

-- | Every node of the tree, depth first.
flatten :: CCSNode -> [CCSNode]
flatten n = n : concatMap flatten (ccsChildren n)

-- | Entries summed over the whole tree.
totalEntries :: CCSNode -> Word64
totalEntries = sum . map ccsEntries . flatten

-- | Bytes allocated summed over the whole tree.
totalAllocBytes :: CCSNode -> Word64
totalAllocBytes = sum . map ccsAllocBytes . flatten
