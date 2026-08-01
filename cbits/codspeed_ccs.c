#include "codspeed_ccs.h"

#include "Rts.h"

/* GHC passes -DPROFILING when compiling c-sources the profiling way. */
#if defined(PROFILING)

int hs_codspeed_ccs_available(void) { return 1; }

uint32_t hs_codspeed_ccs_word_size(void) { return (uint32_t)sizeof(StgWord); }

void *hs_codspeed_ccs_root(void) { return (void *)CCS_MAIN; }

int64_t hs_codspeed_ccs_id(void *ccs) {
  return (int64_t)((CostCentreStack *)ccs)->ccsID;
}

void *hs_codspeed_ccs_cc(void *ccs) {
  return (void *)((CostCentreStack *)ccs)->cc;
}

uint64_t hs_codspeed_ccs_entries(void *ccs) {
  return (uint64_t)((CostCentreStack *)ccs)->scc_count;
}

uint64_t hs_codspeed_ccs_alloc_words(void *ccs) {
  return (uint64_t)((CostCentreStack *)ccs)->mem_alloc;
}

void *hs_codspeed_ccs_children(void *ccs) {
  return (void *)((CostCentreStack *)ccs)->indexTable;
}

void *hs_codspeed_it_next(void *it) {
  return (void *)((IndexTable *)it)->next;
}

void *hs_codspeed_it_ccs(void *it) { return (void *)((IndexTable *)it)->ccs; }

int hs_codspeed_it_back_edge(void *it) {
  return ((IndexTable *)it)->back_edge ? 1 : 0;
}

#else /* not PROFILING */

/* Stubs so the module links in the vanilla way. Callers gate on
 * hs_codspeed_ccs_available(); these exist only to satisfy the linker. */

int hs_codspeed_ccs_available(void) { return 0; }
uint32_t hs_codspeed_ccs_word_size(void) { return (uint32_t)sizeof(StgWord); }
void *hs_codspeed_ccs_root(void) { return NULL; }
int64_t hs_codspeed_ccs_id(void *ccs) { (void)ccs; return 0; }
void *hs_codspeed_ccs_cc(void *ccs) { (void)ccs; return NULL; }
uint64_t hs_codspeed_ccs_entries(void *ccs) { (void)ccs; return 0; }
uint64_t hs_codspeed_ccs_alloc_words(void *ccs) { (void)ccs; return 0; }
void *hs_codspeed_ccs_children(void *ccs) { (void)ccs; return NULL; }
void *hs_codspeed_it_next(void *it) { (void)it; return NULL; }
void *hs_codspeed_it_ccs(void *it) { (void)it; return NULL; }
int hs_codspeed_it_back_edge(void *it) { (void)it; return 0; }

#endif /* PROFILING */
