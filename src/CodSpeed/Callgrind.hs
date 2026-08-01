{- | Just enough of the Callgrind format to take a profile apart and put it back
together.

Not a general parser. It handles what CodSpeed's runner actually produces —
@--compress-strings=no@, @--dump-line=no@, @--combine-dumps=yes@, @positions:
line@ — and it is deliberately conservative everywhere else: anything it does not
recognise is carried through verbatim, so a file it does not fully understand
still round-trips byte-for-byte.

Format reference is @callgrind/docs/cl-format.xml@ in the Valgrind tree. The parts
that matter here:

* A file is a preamble followed by one or more @part:@ blocks.
* Within a part, @fn=@ opens a function. Cost lines under it are that function's
  __self__ cost.
* @cfn=@ names a callee and @calls=@ introduces it; the /single/ cost line that
  follows a @calls=@ is the __inclusive__ cost of that call, not self cost. Getting
  this wrong double-counts everything, so it is the one piece of state the parser
  really has to keep.
* A cost line is a position followed by one number per @events:@ column. The
  position may be absolute, relative (@+3@, @-2@) or @*@; none of that matters
  here, because we only ever sum whole functions.
-}
module CodSpeed.Callgrind (
  -- * Profiles
  Profile (..),
  Part (..),
  partTotals,
  parseProfile,
  renderProfile,

  -- * Costs
  Cost,
  addCost,
  scaleCost,
  zeroCost,
  totalOf,

  -- * Reading a part
  partTrigger,
  selfCostByFunction,
) where

import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

{- | One cost vector: one entry per column of the part's @events:@ line.

Kept as a plain list because the column count is whatever the profile says (14
under CodSpeed's flag set) and nothing here needs to know what the columns mean.
-}
type Cost = [Integer]

zeroCost :: Int -> Cost
zeroCost n = replicate n 0

-- | Pointwise, tolerating different lengths by padding with zeroes.
addCost :: Cost -> Cost -> Cost
addCost = go
  where
    go [] bs = bs
    go as [] = as
    go (a : as) (b : bs) = (a + b) : go as bs

{- | Multiply by a rational @num/den@, rounding down.

Down rather than nearest, so a distribution can never exceed the total it is
dividing; the remainder is handed to a designated node by the caller.
-}
scaleCost :: Integer -> Integer -> Cost -> Cost
scaleCost _ 0 c = map (const 0) c
scaleCost num den c = map (\x -> x * num `div` den) c

-- | Sum of a cost vector's first column, which is @Ir@ under CodSpeed's flags.
totalOf :: Cost -> Integer
totalOf (x : _) = x
totalOf [] = 0

{- | A part, kept as its header lines, its body lines and its @totals:@.

The header and totals are carried verbatim: rewriting a body must not perturb
either, and the surest way to guarantee that is never to reconstruct them.
-}
data Part = Part
  { partHeader :: [String]
  -- ^ Everything from @part:@ down to and including @summary:@.
  , partBody :: [String]
  {- ^ Everything after the header, verbatim — cost entries, the @totals:@ line
  and whatever blank lines separate them.

  Kept whole rather than split into "entries" and "totals" so that rendering is
  concatenation and the round-trip is exact. An earlier version lifted @totals:@
  out into its own field and put it back at the end, which produced a file of
  precisely the same length with two lines swapped.
  -}
  }
  deriving (Show, Eq)

data Profile = Profile
  { profilePreamble :: [String]
  -- ^ Everything before the first @part:@.
  , profileParts :: [Part]
  }
  deriving (Show, Eq)

{- | Split a profile into parts. Never fails: a file with no @part:@ line comes
back as a preamble and nothing else.
-}
parseProfile :: String -> Profile
parseProfile input = case break isPart (lines input) of
  (pre, rest) -> Profile pre (chunk rest)
  where
    isPart = ("part:" `isPrefixOf`)

    chunk [] = []
    chunk (p : rest) =
      let (body, more) = break isPart rest
          (hdr, entries) = splitHeader body
       in Part (p : hdr) entries : chunk more

    -- The header runs to the summary line; everything after is cost entries.
    splitHeader ls = case break ("summary:" `isPrefixOf`) ls of
      (before, s : after) -> (before <> [s], after)
      (before, []) -> (before, [])

{- | Inverse of 'parseProfile'.

Exact for anything 'parseProfile' produced, which is what makes a rewrite
trustworthy: whatever this tool does not deliberately change, it does not change.
-}
renderProfile :: Profile -> String
renderProfile (Profile pre parts) =
  unlines (pre <> concatMap render parts)
  where
    render (Part hdr body) = hdr <> body

-- | The part's @totals:@ line, if it has one.
partTotals :: Part -> Maybe String
partTotals = lookupLine "totals:"

lookupLine :: String -> Part -> Maybe String
lookupLine pfx p = case filter (pfx `isPrefixOf`) (partBody p) of
  (l : _) -> Just l
  [] -> Nothing

{- | The benchmark URI a part was dumped for, from its
@desc: Trigger: Client Request: \<uri\>@ line.

'Nothing' for the parts that are not benchmarks — the metadata dump and the
program-termination dump both land in the same file.
-}
partTrigger :: Part -> Maybe String
partTrigger p =
  case [s | l <- partHeader p, Just s <- [stripPrefix triggerPrefix l]] of
    (s : _) -> Just s
    [] -> Nothing
  where
    triggerPrefix = "desc: Trigger: Client Request: "

{- | Total self cost per function name in a part.

Self cost only: the cost line following a @calls=@ is that call's /inclusive/
cost and is skipped, or every caller would be charged its callees twice over.
-}
selfCostByFunction :: Part -> Map String Cost
selfCostByFunction = snd . foldl step (initial, M.empty) . partBody
  where
    initial = (Nothing, False)

    step ((current, afterCalls), acc) line
      | Just fn <- stripPrefix "fn=" line = ((Just fn, False), acc)
      -- cfn=/cfi=/cob= describe the callee about to be introduced by calls=;
      -- they must not change which function self cost is attributed to.
      | "cfn=" `isPrefixOf` line = ((current, afterCalls), acc)
      | "cfi=" `isPrefixOf` line = ((current, afterCalls), acc)
      | "cfl=" `isPrefixOf` line = ((current, afterCalls), acc)
      | "cob=" `isPrefixOf` line = ((current, afterCalls), acc)
      | "calls=" `isPrefixOf` line = ((current, True), acc)
      | isCostLine line =
          if afterCalls
            then ((current, False), acc)
            else
              ( (current, False)
              , case current of
                  Just fn -> M.insertWith addCost fn (costOf line) acc
                  Nothing -> acc
              )
      | otherwise = ((current, afterCalls), acc)

-- | A cost line starts with a position: a digit, @+@, @-@ or @*@.
isCostLine :: String -> Bool
isCostLine (c : _) = isDigit c || c == '+' || c == '-' || c == '*'
isCostLine [] = False

-- | The numbers on a cost line, dropping the leading position field.
costOf :: String -> Cost
costOf = map readInt . drop 1 . words
  where
    readInt s = case reads s of
      [(n, "")] -> n
      _ -> 0
