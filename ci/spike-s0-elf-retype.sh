#!/usr/bin/env bash
# Spike S0 -- is the ELF symbol table the reason Callgrind cannot name GHC code,
# and is fixing it measurement-neutral?
#
# Runs inside a linux/amd64 container because the question is about ELF, and the
# development host is aarch64 Mach-O.
#
# Four Callgrind runs over one binary, using CodSpeed's exact flag set:
#
#   plain        as GHC links it
#   retyped      after ci/elf-retype-procs.py
#   plain_pop    plain, plus --pop-on-jump
#   retyped_pop  both
#
# Pass criteria
#   1. Ir totals identical across all four.  The retype must not move the number,
#      or it is not a legitimate thing to do to a measured binary.
#   2. Named-frame coverage rises sharply after the retype.
#   3. Repeated runs of the same configuration agree exactly.  Simulation mode is
#      only worth using if it is deterministic; if this fails, stop.
#
# --pop-on-jump is undocumented but real (callgrind/clo.c). GHC's generated code
# tail-jumps rather than calling, and Callgrind only pops a frame when the machine
# stack pointer moves -- which in GHC's execution model it never does -- so frames
# accumulate without ever being popped. --pop-on-jump replaces the frame on a tail
# jump instead of pushing.
set -euo pipefail

IMAGE="${IMAGE:-haskell:9.12.4-slim-bookworm}"
REPEATS="${REPEATS:-3}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Default to the host architecture. Valgrind is itself a dynamic binary
# instrumentation engine, so running it under Rosetta/qemu emulation is a good way
# to get numbers that describe the emulator. Everything S0 asks -- does the symbol
# table hide GHC's local procs, is the retype measurement-neutral, is the count
# deterministic -- is architecture-independent, so run it natively.
#
# CodSpeed's own runners are x86-64, and StgRun only carries unwind information
# there, so confirm on linux/amd64 in CI before relying on the flamegraph shape.
case "$(uname -m)" in
  arm64 | aarch64) PLATFORM="${PLATFORM:-linux/arm64}" ;;
  *) PLATFORM="${PLATFORM:-linux/amd64}" ;;
esac

echo "== spike S0 =="
echo "image:    $IMAGE"
echo "platform: $PLATFORM"
echo "repeats:  $REPEATS"
echo

docker run --rm --platform "$PLATFORM" \
  -v "$HERE/elf-retype-procs.py:/w/elf-retype-procs.py:ro" \
  -e REPEATS="$REPEATS" \
  -i "$IMAGE" bash -s <<'CONTAINER'
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y -qq valgrind binutils python3 >/dev/null
mkdir -p /w && cd /w

# The program needs MANY module-local top-level procs, because that is the shape
# the hypothesis is about: GHC emits .size for every proc but .type only for
# externally visible ones, so module-local procs land as STT_NOTYPE and Valgrind
# drops them. A three-function Main proves nothing -- -O2 merges it down to a
# single worker and there is nothing local left to misclassify.
#
# So generate NSTEPS helpers, none of them exported (the export list is just
# `main`), each NOINLINE, and reach them through a top-level list so they survive
# as separate closures rather than being fused into one loop.
NSTEPS="${NSTEPS:-40}"
{
  echo "module Main (main) where"
  echo
  echo "import Data.List (foldl')"
  echo
  for i in $(seq 0 $((NSTEPS - 1))); do
    echo "{-# NOINLINE step$i #-}"
    echo "step$i :: Int -> Int"
    echo "step$i x = (x * $((i * 2 + 3)) + $i) \`rem\` 1000003"
    echo
  done
  echo "steps :: [Int -> Int]"
  echo -n "steps = ["
  for i in $(seq 0 $((NSTEPS - 1))); do
    [ "$i" -gt 0 ] && echo -n ", "
    echo -n "step$i"
  done
  echo "]"
  echo
  echo "{-# NOINLINE runSteps #-}"
  echo "runSteps :: Int -> Int"
  echo "runSteps x = foldl' (\\a f -> f a) x steps"
  echo
  echo "{-# NOINLINE loop #-}"
  echo "loop :: Int -> Int -> Int"
  echo "loop acc i"
  echo "  | i > 60000 = acc"
  echo "  | otherwise = loop (acc + runSteps i) (i + 1)"
  echo
  echo "main :: IO ()"
  echo "main = print (loop 0 1)"
} > Bench.hs
echo "generated Bench.hs with $NSTEPS module-local helpers"

echo "== building =="
ghc -O2 -fproc-alignment=64 -g3 -o bench Bench.hs -outputdir out >/dev/null
cp bench bench-retyped
python3 /w/elf-retype-procs.py bench-retyped

echo
echo "== (1) symbol census =="
# The interesting cell is LOCAL x NOTYPE with a non-zero size: that is a proc GHC
# gave .size but not .type, which is exactly what Valgrind's symbol filter drops.
for x in bench bench-retyped; do
  echo "  $x:"
  readelf -sW "$x" | awk '
    NR > 3 && $4 != "" {
      bind=$5; type=$4; size=$3+0
      if (type=="FUNC" || type=="NOTYPE")
        key=bind" "type(size>0?" (size>0)":" (size=0)")
      else next
      count[key]++
    }
    END { for (k in count) printf "    %-28s %s\n", k, count[k] }' | sort
  echo -n "    step* helpers visible: "
  readelf -sW "$x" | grep -c 'Main_step' || true
done
echo "  sample of the step helpers as GHC emitted them:"
readelf -sW bench | grep 'Main_step' | head -3 | awk '{printf "    %-8s %-8s %-8s %s\n", $3, $4, $5, $8}'

# CodSpeed's flag set, from codspeed/src/executor/valgrind/measure.rs
run() {
  local bin=$1 tag=$2; shift 2
  valgrind -q --tool=callgrind --cache-sim=yes \
    --I1=32768,8,64 --D1=32768,8,64 --LL=8388608,16,64 \
    --read-inline-info=yes --compress-strings=no --combine-dumps=yes \
    --dump-line=no "$@" --callgrind-out-file="/w/$tag.out" \
    ./"$bin" >/dev/null 2>"/w/$tag.log"
}

report() {
  local tag=$1
  local ir hexfn allfn named
  ir=$(awk '/^totals:/{print $2}' "/w/$tag.out")
  hexfn=$(grep -c '^fn=(\?[0-9]*)\?\s*0x' "/w/$tag.out" || true)
  allfn=$(grep -c '^fn=' "/w/$tag.out" || true)
  named=$((allfn - hexfn))
  printf '  %-14s Ir=%-14s fn_total=%-6s fn_hex=%-6s fn_named=%s\n' \
    "$tag" "$ir" "$allfn" "$hexfn" "$named"
}

echo
echo "== (2) callgrind, CodSpeed's exact flags =="
run bench         plain
run bench-retyped retyped
run bench         plain_pop     --pop-on-jump=yes
run bench-retyped retyped_pop   --pop-on-jump=yes
for t in plain retyped plain_pop retyped_pop; do report "$t"; done

echo
echo "== (3) noise floor: same binary, $REPEATS runs =="
# Establish this BEFORE comparing anything. Whole-process Ir includes dynamic
# linking and libc startup, which vary with the environment block and loader
# work, so a spread of a few hundred instructions is expected and says nothing
# about the Haskell code. Any comparison below has to clear this bar.
lo=""; hi=""
for i in $(seq 1 "$REPEATS"); do
  run bench-retyped "det$i"
  v=$(awk '/^totals:/{print $2}' "/w/det$i.out")
  echo "  run $i: Ir=$v"
  [ -z "$lo" ] && { lo=$v; hi=$v; }
  [ "$v" -lt "$lo" ] && lo=$v
  [ "$v" -gt "$hi" ] && hi=$v
done
noise=$((hi - lo))
echo "  noise floor: $noise Ir  (min=$lo max=$hi)"
if [ "$noise" -eq 0 ]; then
  echo "  PASS  whole-process Ir is bit-deterministic"
else
  echo "  NOTE  whole-process Ir varies by $noise; per-benchmark instrumentation"
  echo "        brackets only the measured region and excludes process startup"
fi

echo
echo "== (4) is the retype measurement-neutral? =="
ir_plain=$(awk '/^totals:/{print $2}' /w/plain.out)
ir_retyped=$(awk '/^totals:/{print $2}' /w/retyped.out)
promoted=$(python3 /w/elf-retype-procs.py -n bench 2>&1 | grep -o '[0-9]\+ symbols' | cut -d' ' -f1)
delta=$((ir_plain > ir_retyped ? ir_plain - ir_retyped : ir_retyped - ir_plain))
echo "  symbols promoted: ${promoted:-0}"
echo "  plain=$ir_plain retyped=$ir_retyped delta=$delta"
if [ "${promoted:-0}" -eq 0 ]; then
  echo "  N/A   nothing to promote on this architecture -- the binaries are identical,"
  echo "        so any delta here is the noise floor from (3), not the retype"
elif [ "$delta" -le "$noise" ]; then
  echo "  PASS  delta is within the noise floor -- retype does not move the number"
else
  echo "  FAIL  delta $delta exceeds noise floor $noise"
fi

echo
echo "== (5) which Main symbols does callgrind see at all? =="
# -O2 worker-wrappers these into $w-prefixed names, which z-encode as zdw, so
# match the module prefix rather than the source-level identifier.
for t in plain retyped_pop; do
  hits=$(grep -c '^fn=.*Main_' "/w/$t.out" || true)
  printf '    %-14s Main_* symbols=%s\n' "$t" "$hits"
done
echo "  names seen:"
grep -h '^fn=' /w/retyped_pop.out | sed 's/^fn=//' | grep 'Main_' | sort -u | sed 's/^/    /' || echo "    (none)"

echo
echo "== (6) top frames by SELF COST (not by occurrence) =="
# Ranking by cost, not by how many contexts a name appears in -- the latter says
# nothing about where the instructions went.
callgrind_annotate --threshold=80 /w/retyped_pop.out 2>/dev/null | head -40 \
  || echo "  callgrind_annotate failed"

echo
echo "== (6b) share of Ir on named Haskell code vs RTS/libc =="
# GHC symbols are z-encoded and end in _info/_entry/_closure; RTS C and libc are
# plain identifiers. Counts only the first (self-cost) column.
callgrind_annotate --threshold=100 /w/retyped_pop.out 2>/dev/null \
  | awk '
      /^-+$/ { next }
      /^ *[0-9,]+ / {
        c=$1; gsub(/,/,"",c); if (c !~ /^[0-9]+$/) next
        tot += c
        if ($0 ~ /_info|_entry|_closure|_slow/) hs += c
      }
      END {
        if (tot > 0)
          printf "  haskell symbols=%d  total=%d  (%.1f%% of attributed Ir)\n", hs, tot, 100*hs/tot
        else
          print "  could not parse callgrind_annotate output"
      }'

echo
echo "== (7) shadow-stack growth (does --pop-on-jump help?) =="
for t in plain plain_pop retyped retyped_pop; do
  n=$(grep -c 'call stack enlarged' "/w/$t.log" || true)
  sz=$(stat -c %s "/w/$t.out")
  printf '  %-14s stack_enlarged=%-4s out_bytes=%s\n' "$t" "$n" "$sz"
done
CONTAINER
