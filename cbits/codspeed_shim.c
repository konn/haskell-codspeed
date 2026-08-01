#include "codspeed_shim.h"

#include <HsFFI.h>
#include <Rts.h>

#ifdef _WIN32
#  include <process.h>
#  define hs_codspeed_getpid_impl _getpid
#else
#  include <unistd.h>
#  define hs_codspeed_getpid_impl getpid
#endif

#if defined(_MSC_VER)
#  define CODSPEED_NOINLINE __declspec(noinline)
#else
#  define CODSPEED_NOINLINE __attribute__((noinline))
#endif

/* Defined by the `foreign export ccall` in CodSpeed.Instrument.RootFrame. */
extern void hs_codspeed_run_action(HsStablePtr action);

uint8_t hs_codspeed_start_benchmark(InstrumentHooks *hooks) {
  return instrument_hooks_start_benchmark_inline(hooks);
}

uint8_t hs_codspeed_stop_benchmark(InstrumentHooks *hooks) {
  return instrument_hooks_stop_benchmark_inline(hooks);
}

int32_t hs_codspeed_getpid(void) { return (int32_t)hs_codspeed_getpid_impl(); }

bool hs_codspeed_nonmoving_gc(void) { return RtsFlags.GcFlags.useNonmoving; }

/* Must not be inlined: the whole point is that this frame is observable, by
 * name, on the native stack while the benchmark runs. */
CODSPEED_NOINLINE void __codspeed_root_frame__hsBench(void *action) {
  hs_codspeed_run_action((HsStablePtr)action);
}
