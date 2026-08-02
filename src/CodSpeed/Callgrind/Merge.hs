{- | __Experimental.__ Grafting a cost-centre tree onto measured Callgrind costs.

Off unless @CODSPEED_HS_CCS_DIR@ is set for the measured run, and it should stay
off until the caveat at the end of this header is resolved.

== Why no other integration does this

Every CodSpeed integration that renders a flamegraph does it by making the
language's frames into /real code addresses/ Valgrind can resolve, never by
authoring a profile:

* C++, Rust, Zig and Go are natively compiled, so their frames already are ELF
  symbols.
* Python emits a CPython perf map — @\/tmp\/perf-\<pid\>.map@ — giving each
  interpreted function an address range. That is what CodSpeed's
  @MissingPerfMap@ failure means when a run reports itself as @pytest-codspeed@.
* Node does the same through V8.

Haskell can join neither group. A cost centre is not a code address:
@-fprof-late@ maintains the cost-centre stack at runtime, and one address sits
beneath many different cost-centre stacks, so there is no perf map to write.
Authoring the profile is the only route left, and it is one the backend has never
been asked to accept.

The problem this solves: at @-O2@ a Haskell function often has no frame of its
own in the ELF symbol table. The lazy @fib@'s accumulator loop spends its time in
@stg_upd_frame_info@, @stg_BLACKHOLE_info@ and @threadPaused@ — thunk-entry and
update-frame machinery — so a flamegraph built from symbols shows the runtime
doing thunk bookkeeping and never names @Main.$wgo@. Decoding the symbols does not
help, because the frame genuinely is not there.

GHC's cost-centre tree does name it, and this module puts the two together:
measured cost from the Valgrind run, shape from a @-fprof-late@ side-car.

== What is measured and what is modelled

__Measured, and preserved exactly:__ the part's @totals:@, and the split between
Haskell code and the runtime. Every cost that Callgrind attributed to an RTS or
libc symbol stays on an RTS or libc frame, so "most of this benchmark was GC"
survives the rewrite intact and is still true afterwards.

__Modelled:__ how the Haskell share is divided among cost centres. That
distribution comes from the side-car's allocation weights, not from the
instruction counts, because instruction counts per cost centre do not exist —
that is the whole difficulty. Allocation is a good proxy for a lazy program's
time and a poor one for a tight arithmetic loop, and the flamegraph says so: the
modelled subtree hangs under a frame named for what it is, so nobody reads it as
a measurement.

The honest one-line summary is that the /shape/ is inferred and the /totals/ are
not.

== Why this is sound enough to be worth doing

Both runs execute the benchmark body exactly once — the side-car only does so
because @CODSPEED_HS_DETERMINISTIC@ makes it take the same path the instrumented
run takes. That is what makes the two comparable at all, and it is checkable:
the side-car's allocation figure should match the measured run's, and it does
(676592 B for @fib.10000.leaky@ on both, across two architectures).

== Why it is still experimental

__CodSpeed does not render the authored graph, and the metric moves.__ Measured
in one run, same commit, same binary, merged against rename-only:

@
                     rename-only   merged
fib.10000.leaky        3.5 ms      4.4 ms
fib.10000.strict      116.5 us    371.5 us
@

and the flamegraph collapses to a single frame at 100%.

The second line of that table is the important one, and it invalidates the
obvious way to test this module. Every part it emits reconciles exactly against
its own @totals:@, and @totals:@ is byte-identical before and after — but the
number CodSpeed reports is derived from the /graph/, not from @totals:@. So
internal reconciliation, which is what the tests check, cannot detect a metric
regression. Restoring @ob=@ and @fl=???@ — the only structural difference from
the profile that does render — changed nothing.
-}
module CodSpeed.Callgrind.Merge (
  -- * Cost-centre trees
  CCTree (..),
  parseFolded,
  treeSelfByName,
  treeEdges,
  treeTotal,

  -- * Classifying measured frames
  Bucket (..),
  bucketOf,

  -- * Merging
  mergePart,
) where

import CodSpeed.Callgrind
import CodSpeed.Callgrind.Demangle (demangleSymbol)
import Data.List (isInfixOf, isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

{- | A cost-centre tree, as read from folded stacks.

'ccSelf' is the weight of paths ending exactly at this node; a node's inclusive
weight is that plus its children's.
-}
data CCTree = CCTree
  { ccName :: String
  , ccSelf :: !Integer
  , ccChildren :: [CCTree]
  }
  deriving (Show, Eq)

{- | Read folded stacks: @frame;frame;frame WEIGHT@ per line.

Malformed lines are skipped rather than fatal — this runs inside a benchmark
pipeline, and losing a frame is better than losing the run.
-}
parseFolded :: String -> [CCTree]
parseFolded = foldl' (\acc (p, w) -> insertPath p w acc) [] . concatMap parseLine . lines
  where
    -- The weight is the last space-separated field; the frames are everything
    -- before it. Split from the right, because a cost-centre name may contain
    -- spaces (a benchmark called "fused (allocates nothing)" does).
    parseLine l = case span (/= ' ') (reverse l) of
      (revW, ' ' : revPath) -> case reads (reverse revW) of
        [(w, "")] | w > 0 -> [(splitOn ';' (reverse revPath), w :: Integer)]
        _ -> []
      _ -> []

{- | Add one folded path to a forest, merging into an existing node when the name
is already there at that level.

Linear per level, which is fine: a folded profile of a single benchmark is a few
hundred lines.
-}
insertPath :: [String] -> Integer -> [CCTree] -> [CCTree]
insertPath [] _ ts = ts
insertPath (f : rest) w ts = case break ((== f) . ccName) ts of
  (before, t : after) -> before <> (update t : after)
  (before, []) -> before <> [update (CCTree f 0 [])]
  where
    update t
      | null rest = t {ccSelf = ccSelf t + w}
      | otherwise = t {ccChildren = insertPath rest w (ccChildren t)}

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest

-- | Total weight of a forest.
treeTotal :: [CCTree] -> Integer
treeTotal = sum . map incl

incl :: CCTree -> Integer
incl t = ccSelf t + sum (map incl (ccChildren t))

{- | Self weight per cost-centre name, summed over every position it occupies.

Callgrind aggregates by function name, so this is the shape its model wants: one
@fn=@ per name, however many places the tree mentions it.
-}
treeSelfByName :: [CCTree] -> Map String Integer
treeSelfByName = foldl' go M.empty
  where
    go acc t =
      foldl' go (M.insertWith (+) (ccName t) (ccSelf t) acc) (ccChildren t)

{- | Caller/callee edges: how many times, and with what inclusive weight.

Computed over the /tree/ and only then aggregated, so recursion — a path like
@A;B;A@ — cannot send this into a loop the way walking the aggregated graph
would.
-}
treeEdges :: [CCTree] -> Map (String, String) (Int, Integer)
treeEdges = foldl' go M.empty
  where
    go acc t = foldl' (child (ccName t)) (foldl' go acc (ccChildren t)) (ccChildren t)
    child parent acc c =
      M.insertWith merge (parent, ccName c) (1, incl c) acc
    merge (n1, w1) (n2, w2) = (n1 + n2, w1 + w2)

{- | Where a measured symbol's cost belongs once cost centres take over the
Haskell frames.

Deliberately coarse. The point is not to describe the runtime in detail — the
symbol-level profile already does that — but to keep the runtime's /share/ honest
and legible next to a modelled Haskell subtree.
-}
data Bucket
  = -- | A GHC module symbol; its cost joins the modelled cost-centre subtree.
    Haskell
  | -- | Collector: marking, scavenging, evacuating, the nonmoving collector.
    RtsGC
  | -- | Heap allocation and block management.
    RtsAlloc
  | -- | Thunk entry, update frames, generic apply, the Haskell stack.
    RtsStack
  | -- | Scheduler, threads, STM, in-calls.
    RtsScheduler
  | -- | libc, the dynamic linker, syscall stubs.
    Libc
  | -- | Anything else, including this package's own shim.
    Other
  deriving (Show, Eq, Ord)

-- | The frame a bucket is emitted as.
bucketFrame :: Bucket -> String
bucketFrame b = case b of
  Haskell -> "<haskell>"
  RtsGC -> "<RTS: garbage collector>"
  RtsAlloc -> "<RTS: allocation>"
  RtsStack -> "<RTS: thunks and stack>"
  RtsScheduler -> "<RTS: scheduler>"
  Libc -> "<libc and dynamic linker>"
  Other -> "<other runtime>"

{- | Classify a Callgrind symbol.

Order matters: the GC test comes before the general @stg_@ test because
@stg_gc_*@ is stack overflow handling rather than collection, and the RTS's own
collector functions carry no prefix at all.
-}
bucketOf :: String -> Bucket
bucketOf sym
  | demangleSymbol sym /= sym = Haskell
  | any (`isPrefixOf` sym) gcNames || "nonmoving" `isInfixOf` sym = RtsGC
  | any (`isPrefixOf` sym) allocNames = RtsAlloc
  | any (`isPrefixOf` sym) schedNames = RtsScheduler
  | "stg_" `isPrefixOf` sym || "__stg_" `isPrefixOf` sym || "StgRun" `isPrefixOf` sym = RtsStack
  | any (`isPrefixOf` sym) libcNames = Libc
  | otherwise = Other
  where
    gcNames =
      [ "GarbageCollect"
      , "evacuate"
      , "scavenge"
      , "mark_"
      , "markQueue"
      , "performGC"
      , "performMajorGC"
      , "copy_tag"
      , "todo_block"
      , "dirty_"
      , "push_"
      , "trace_"
      ]
    allocNames = ["allocate", "allocGroup", "allocBlock", "newCAF", "stg_newPinned"]
    schedNames =
      [ "schedule"
      , "thread"
      , "setTSO"
      , "rts_"
      , "hs_"
      , "stm"
      , "maybePerformBlocked"
      , "updateAdjacent"
      , "createThread"
      , "freeGroup"
      ]
    libcNames = ["__", "_dl_", "_int_", "malloc", "free", "memcpy", "memset", "strlen"]

{- | Rewrite one benchmark part's body as a cost-centre tree plus runtime buckets.

Returns 'Nothing' when there is nothing to merge — no cost-centre profile for
this benchmark, or a part with no costs — in which case the caller should leave
the part exactly as it found it.

The emitted body satisfies, by construction: sum of all self costs equals the
part's measured total, per event column. The remainder from integer division is
given to the root frame, which is the only frame whose cost is bookkeeping rather
than a claim about the program.
-}
mergePart :: [CCTree] -> Part -> Maybe [String]
mergePart forest part
  | null forest = Nothing
  | totalWeight <= 0 = Nothing
  | all (== 0) measuredTotal = Nothing
  | otherwise = Just (bodyLines <> ["", totalsLine])
  where
    selfs = selfCostByFunction part
    width = maximum (1 : map length (M.elems selfs))

    -- Split the measured self cost by where it belongs.
    byBucket :: Map Bucket Cost
    byBucket =
      M.fromListWith addCost [(bucketOf f, c) | (f, c) <- M.toList selfs]

    measuredTotal = foldr addCost (zeroCost width) (M.elems byBucket)
    haskellShare = M.findWithDefault (zeroCost width) Haskell byBucket
    runtimeBuckets = M.toList (M.delete Haskell byBucket)

    totalWeight = treeTotal forest
    selfByName = treeSelfByName forest
    edges = treeEdges forest

    -- Cost centres get the Haskell share, split by allocation weight.
    ccCost w = scaleCost w totalWeight haskellShare

    emittedSelf =
      foldr
        addCost
        (zeroCost width)
        ([ccCost w | w <- M.elems selfByName] <> map snd runtimeBuckets)

    -- Integer division loses a little; the root frame absorbs it so the part
    -- still reconciles exactly with its own totals: line.
    remainder = zipWithLong (-) measuredTotal emittedSelf

    rootName = "__codspeed_root_frame__hsBench"
    modelledName = "<cost centres: modelled, see haskell-codspeed>"

    -- Callgrind never emits a body without an ob=, and a rewritten one must not
    -- either: a frame that belongs to no object is not expressible in the
    -- format. Taken from the measured part rather than invented, preferring
    -- whichever object the heaviest function came from -- which is the binary
    -- under test, not libc.
    --
    -- fl=??? with three question marks, which is what Callgrind writes for an
    -- unknown file; the first version wrote two.
    context =
      [ "ob=" <> obj
      | Just obj <- [heaviestObject]
      ]
        <> ["fl=???"]

    heaviestObject =
      let objs = objectByFunction part
       in case [ o
               | (f, _) <- take 1 (sortOn (negate . totalOf . snd) (M.toList selfs))
               , Just o <- [M.lookup f objs]
               ] of
            (o : _) -> Just o
            [] -> case M.elems objs of
              (o : _) -> Just o
              [] -> Nothing

    bodyLines =
      concat
        [ context <> ["fn=" <> rootName, costLine remainder]
        , -- The modelled subtree, announced by the frame it hangs from.
          concat
            [ ["cfn=" <> modelledName, "calls=1 0", costLine (ccCost totalWeight)]
            | totalOf haskellShare > 0
            ]
        , concat
            [ ["cfn=" <> bucketFrame b, "calls=1 0", costLine c]
            | (b, c) <- runtimeBuckets
            ]
        , -- The announcing frame itself costs nothing; it exists to be a label.
          concat
            [ ["", "fn=" <> modelledName, costLine (zeroCost width)]
                <> concat
                  [ ["cfn=" <> ccName t, "calls=1 0", costLine (ccCost (incl t))]
                  | t <- forest
                  ]
            | totalOf haskellShare > 0
            ]
        , concatMap ccFrame (M.toList selfByName)
        , concat [["", "fn=" <> bucketFrame b, costLine c] | (b, c) <- runtimeBuckets]
        ]

    ccFrame (name, w) =
      ["", "fn=" <> name, costLine (ccCost w)]
        <> concat
          [ ["cfn=" <> callee, "calls=" <> show n <> " 0", costLine (ccCost w')]
          | ((caller, callee), (n, w')) <- M.toList edges
          , caller == name
          ]

    totalsLine = "totals: " <> unwords (map show measuredTotal)

costLine :: Cost -> String
costLine c = "0 " <> unwords (map show c)

zipWithLong :: (Integer -> Integer -> Integer) -> Cost -> Cost -> Cost
zipWithLong f xs ys = go xs ys
  where
    go [] bs = map (f 0) bs
    go as [] = map (`f` 0) as
    go (a : as) (b : bs) = f a b : go as bs
