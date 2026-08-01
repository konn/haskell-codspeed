/* Access to GHC's cost-centre stack tree.
 *
 * base's GHC.Stack.CCS exposes ccsCC, ccsParent, ccLabel, ccModule and
 * ccSrcSpan, which is enough to describe a single stack but not to walk the
 * tree: the child IndexTable and the per-node counters are not reachable from
 * Haskell. These accessors supply the rest.
 *
 * They are accessors rather than hsc2hs #peeks on purpose. CostCentreStack
 * carries explicit alignment requirements (see "Note [struct alignment]" in
 * rts/prof/CCS.h) around its StgWord64 fields, so hand-computed offsets are a
 * standing invitation to read the wrong field on some platform.
 *
 * The whole file compiles in both ways. Without -prof there is no cost-centre
 * tree at all, so every entry point degrades to a null or zero and
 * hs_codspeed_ccs_available() reports 0.
 */

#ifndef HASKELL_CODSPEED_CCS_H
#define HASKELL_CODSPEED_CCS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Non-zero when this object was built the profiling way, i.e. when there is a
 * cost-centre tree to walk at all. */
int hs_codspeed_ccs_available(void);

/* Size of StgWord in bytes. CostCentreStack.mem_alloc counts words, not
 * bytes. */
uint32_t hs_codspeed_ccs_word_size(void);

/* CCS_MAIN, the root of the user-code tree.
 *
 * Deliberately not CCS_SYSTEM/CCS_GC/CCS_OVERHEAD: those are separate roots
 * holding runtime costs, and folding them in would misattribute collector work
 * to whatever Haskell function happened to be running. */
void *hs_codspeed_ccs_root(void);

/* CostCentreStack fields. */
int64_t hs_codspeed_ccs_id(void *ccs);
void *hs_codspeed_ccs_cc(void *ccs);
uint64_t hs_codspeed_ccs_entries(void *ccs);   /* scc_count */
uint64_t hs_codspeed_ccs_alloc_words(void *ccs); /* mem_alloc, in WORDS */
void *hs_codspeed_ccs_children(void *ccs);     /* IndexTable head, may be NULL */

/* IndexTable fields, for iterating a node's children. */
void *hs_codspeed_it_next(void *it);
void *hs_codspeed_it_ccs(void *it);
/* Non-zero when this edge re-enters a cost centre already on the stack. Such
 * edges must not be followed: the tree is not acyclic through them. */
int hs_codspeed_it_back_edge(void *it);

#ifdef __cplusplus
}
#endif

#endif /* HASKELL_CODSPEED_CCS_H */
