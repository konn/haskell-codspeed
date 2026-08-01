# haskell-codspeed

Run GHC benchmarks under [CodSpeed](https://codspeed.io).

CodSpeed has first-party integrations for Rust, Python, Node, Go and C/C++, but not
Haskell. Its zero-code-change "exec harness" does work on a GHC binary, but it measures
whole processes, which for a Haskell benchmark suite means measuring mostly the wrong
thing. This package is the real integration, built on CodSpeed's
[`instrument-hooks`](https://github.com/CodSpeedHQ/instrument-hooks).

## Quick start

Swap the import:

```haskell
-- import Test.Tasty.Bench
import Test.Tasty.Bench.CodSpeed

main :: IO ()
main = defaultMain
  [ bgroup "solve" [ bench "uf20" (nf solve cnf) ] ]
```

Nothing else changes — everything `Test.Tasty.Bench` exports is re-exported, and
without a CodSpeed runner attached the suite behaves exactly as before.

```bash
cabal bench example                                  # ordinary tasty-bench run
codspeed run --mode simulation -- cabal bench example  # per-benchmark measurement
```

## Why per-leaf measurement matters here

Measuring a whole process sweeps in RTS startup, benchmark-tree construction, input
loading and console output. On a real suite that dominated the result completely:

| region | share of the reported metric |
| --- | ---: |
| `performMajorGC` → `nonmovingCollect` | 76% |
| tasty startup, input I/O, console | ~23% |
| the algorithm under test | ~1–2% |

Every one of those numbers was reproducible and plausible. This package opens the
measurement window around each `bench` leaf instead, so the number describes the code
you wrote.

## What it measures

**Instruction counts** come from CodSpeed's CPU-simulation instrument, per benchmark,
reported under a URI derived from the tasty path (`bench/Bench.hs::solve::uf20`).

**Allocation** comes from GHC itself and is tracked as a co-equal metric. It is read
from `getAllocationCounter`, which is per-thread and byte-accurate, so unlike
`GHC.Stats.allocated_bytes` it is not polluted by whatever else the process is doing.
For fixed code and input it is *exactly* reproducible — often a sharper regression
signal than instruction counts, and it works on macOS and Windows where simulation mode
cannot run at all.

## Gating on allocation

Each run can write a CSV of per-benchmark allocation, and `codspeed-hs-compare` fails
CI when it grows. This is independent of CodSpeed — it works on any host, including
ones where simulation mode cannot run.

```bash
CODSPEED_HS_SIDECAR=alloc.csv cabal bench example
codspeed-hs-compare baseline/alloc.csv alloc.csv --tolerance 0.01
```

```
allocation: 1 regression(s) beyond 1.0%
  example::fib::10000::leaky: +20.0% 643416 B -> 772099 B
```

The `fib` benchmarks in [bench/Example.hs](bench/Example.hs) are there to make this
concrete: the same recursion twice, differing only in whether the accumulators are
forced.

```
example::fib::10000::leaky    643416 B
example::fib::10000::strict        0 B
example::fib::1000::leaky      47943 B
example::fib::1000::strict         0 B
```

The strict variant compiles to an unboxed loop and allocates *nothing*; the lazy one
builds a chain of pending additions on the heap. A change that accidentally makes a
strict fold lazy is invisible in a quick timing check and unmissable here.

A benchmark disappearing from the suite also fails, since that is otherwise
indistinguishable from one that stopped regressing.

Only `allocated_bytes` is gated, because only it is reproducible. Two runs of an
identical binary reported 9591818 bytes allocated both times, and 580680 vs 580663
bytes *copied* — copying depends on what the collector chose to do. The other columns
are recorded for diagnosis.

A copy is also dropped into `$CODSPEED_PROFILE_FOLDER` when the runner sets one; that
folder is tarred and uploaded wholesale, so the data rides along for free.

## Haskell cost-centre flamegraphs

Build the suite the profiling way and each benchmark emits a folded-stacks profile
whose frames are Haskell cost centres — not RTS internals:

```bash
cabal build all --enable-profiling --enable-library-profiling \
  --profiling-detail=late-toplevel
CODSPEED_HS_CCS_DIR=ccs ./example
speedscope ccs/example__fib__10000__leaky.folded
```

For the leaky `fib`, the whole profile is the leak:

```
…funcToBenchLoop3;Main.$wgo                                2621120000
…funcToBenchLoop3;Main.$wgo;GHC.Internal.Num.$fNumInt_$c+  1310428944
```

The accumulator loop, and the `Int` addition piling up inside it. Its strict
companion has no `Main` frames in the profile at all — nothing of the benchmark body
reaches the heap.

`-fprof-late` inserts cost centres *after* optimisation, so these name the program
that actually runs. The profile is re-rooted at the benchmark using the cost-centre
stack captured when it starts, which strips the dozen-odd frames of `main` and tasty
scheduling above it.

This needs nothing from CodSpeed and works today. Getting the same shape *inside*
CodSpeed's own flamegraphs is a different problem: those come from Callgrind, whose
frames are ELF symbols, and a cost centre is not a symbol — `-fprof-late` emits
inline `pushCostCentre` and static data, no code label. So no combination of GHC or
Valgrind flags can put one there; the profile would have to be authored, which rests
on an unverified assumption about CodSpeed's closed backend.

## Cooperating with the rest of the toolchain

- **`+RTS -T`** — when present, per-benchmark GC counts and copied bytes are reported
  alongside allocation. A non-zero GC count inside a window is a quality signal: raise
  `-A`.
- **`+RTS -l`** — each benchmark is bracketed with eventlog markers
  (`START codspeed <uri>` / `STOP codspeed <uri>`), so `eventlog2html`, ThreadScope and
  `ghc-events-analyze` can slice a run per benchmark. Emitted on both the CodSpeed and
  the plain path, since local analysis is the main use case. Off unless the eventlog is
  actually on, because `traceEventIO` allocates either way.
- **`-fprof-late`** — see [above](#haskell-cost-centre-flamegraphs). A profiling build
  is deliberately not the thing to measure: `-prof` adds two words to every heap
  object, shifting both instruction counts and allocation. Run it as a side-car
  instead, so the measured binary stays the one you ship.

## RTS hygiene

Wrong RTS flags do not fail loudly; they just measure something else. `defaultMain`
checks at startup and reports on stderr:

| finding | severity |
| --- | --- |
| `--nonmoving-gc` — concurrent marking lands inside windows nondeterministically | critical |
| `-N>1` with parallel GC — collection split nondeterministically | critical |
| small `-A` — collections inside the measured window | advisory |
| RTS timer running (no `-V0`) under simulation | advisory |
| idle GC enabled (no `-I0`) | advisory |
| `-T` absent — no GC statistics | advisory |

A reasonable stanza:

```cabal
ghc-options:
  -O2
  -rtsopts
  "-with-rtsopts=-A32m -T -V0 -I0"
```

Note that `-A` is part of the measurement contract: changing it shifts instruction
counts exactly as a code change would, and invalidates the CodSpeed baseline.

## Caveats worth knowing

**The body runs exactly once under instrumentation.** Nothing is averaged and no first
noisy run is discarded, so `whnf f x` pays for forcing `x` inside the measurement. Force
inputs beforehand — `env` already does.

**`nfIO (pure x)`** lets GHC float `x` out, leaving the window measuring `pure`. Use
`nfAppIO`.

**Fusion can erase your benchmark.** `nf (\k -> sum [1..k]) 100000` compiles at `-O2` to
an unboxed loop that allocates nothing whatsoever; the `example` benchmark demonstrates
this next to a version that does allocate.

**No default timeout under instrumentation.** `tasty-bench` imposes 100 seconds per
benchmark. Simulation is roughly 200× slower, so that is reached by work taking half a
second natively — and the truncation is silent, with the instrument still reporting the
count of the shortened run. An explicit `localOption (mkTimeout n)` is still honoured.

## Layout

| component | contents |
| --- | --- |
| `haskell-codspeed` | `CodSpeed.Instrument`, `CodSpeed.RTS.{Stats,Preflight,Eventlog}` — framework-agnostic |
| `haskell-codspeed:tasty-bench-codspeed` | `Test.Tasty.Bench.CodSpeed`, the drop-in runner |
| `haskell-codspeed:instrument-hooks` | vendored CodSpeed C library, isolated so its warning suppressions do not leak |

The C library is vendored rather than submoduled: `cabal sdist` has no VCS awareness, so
a submodule produces a tarball that cannot build. See
[`vendor/instrument-hooks/README.md`](vendor/instrument-hooks/README.md).

## Status

Working: per-benchmark measurement, allocation tracking and its CI gate, RTS
preflight, eventlog markers, cost-centre flamegraphs as a local artifact, and
`--csv` / `--baseline` / `--svg` / `bcompare` compatibility.

Not yet: cost-centre-shaped flamegraphs **inside CodSpeed's own UI**. Reaching that
means authoring the callgrind profile from GHC cost-centre data, and whether the
backend renders an authored `fn=`/`cfn=`/`calls=` graph — rather than
re-symbolicating from the binary — is untested. It is a short experiment against a
CodSpeed-connected repo; until it is run, the local artifact above is the answer.

Also unverified: instruction-count determinism on real x86-64 hardware. Valgrind
segfaults under emulation on an Apple Silicon host, so the spikes workflow exists to
answer it on a CI runner.
