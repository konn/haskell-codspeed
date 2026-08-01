/* Linkable shims over instrument-hooks, for the Haskell FFI.
 *
 * Two things in core.h cannot be reached by `foreign import ccall`:
 *
 *   - instrument_hooks_{start,stop}_benchmark_inline are `static inline`, so they
 *     have no symbol to import. They are also the only place the Callgrind client
 *     requests are issued, so we cannot simply skip them.
 *   - the __codspeed_root_frame__ requirement is a constraint on a *symbol name*,
 *     and GHC mangles every Haskell binding into <pkg>_<Module>_<occ>_info. Only a
 *     C function can carry that name.
 */

#ifndef HASKELL_CODSPEED_SHIM_H
#define HASKELL_CODSPEED_SHIM_H

#include <stdbool.h>
#include <stdint.h>

#include "core.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Open the measurement window. Mirrors instrument_hooks_start_benchmark_inline.
 * Returns 0 on success.
 *
 * CALLGRIND_START_INSTRUMENTATION is the last thing this does, so every
 * instruction after it returns is counted. Keep the caller tight. */
uint8_t hs_codspeed_start_benchmark(InstrumentHooks *hooks);

/* Close the measurement window. Mirrors instrument_hooks_stop_benchmark_inline. */
uint8_t hs_codspeed_stop_benchmark(InstrumentHooks *hooks);

/* getpid(), without dragging in a dependency on `unix` (and working on Windows,
 * where instrument-hooks itself uses _getpid). */
int32_t hs_codspeed_getpid(void);

/* Is the non-moving (concurrent mark-sweep) collector enabled, i.e. was
 * --nonmoving-gc given?
 *
 * GHC.RTS.Flags does not expose this: GCFlags has no corresponding field, and
 * the `nonmoving_gc` fields that do exist there belong to DebugFlags and
 * TraceFlags and only control tracing. So we read RtsFlags.GcFlags.useNonmoving
 * directly.
 *
 * Worth surfacing because it is measurement-destroying under CPU simulation:
 * the collector marks concurrently on its own thread, so its cost lands
 * nondeterministically inside whichever benchmark window happens to be open. */
bool hs_codspeed_nonmoving_gc(void);

/* Root frame. Calls back into Haskell via the `hs_codspeed_run_action` foreign
 * export, so that the benchmarked code runs with a C frame named
 * __codspeed_root_frame__* beneath it.
 *
 * The window is opened and closed *inside* the callback rather than around this
 * call, so the cost of re-entering the RTS (rts_inCall, bound-thread setup) stays
 * outside the counted region. */
void __codspeed_root_frame__hsBench(void *action);

#ifdef __cplusplus
}
#endif

#endif /* HASKELL_CODSPEED_SHIM_H */
