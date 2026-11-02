# =============================================================================
# Development script - not part of the final analysis
#
# Filter keeping only ties with events in both directions; considered and not
# used.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

### Extract only those ties that are reciprocated ----------------------------

# i.e., the pair (A,B) appears at least once in each direction within
# observation window.

library(tidyverse)

### Build a directed edge lis with call counts -------------------------------

edges_dir <- cdr_internal %>% 
  count(caller, callee, name = "calls_ab")   # weight = # calls A->B

### Keep only pairs that exist in BOTH directions ----------------------------

# Join the table to its reversed copy
edges_recip <- edges_dir %>% 
  inner_join(edges_dir, 
             by = c("caller" = "callee", "callee" = "caller")) %>%  
  # suffix = c("_ab", "_ba")  # _ab = A->B, _ba = B->A
  mutate(
    calls_ab = calls_ab.x, 
    calls_ba = calls_ab.y, 
    calls_total = calls_ab + calls_ba   # optional: combined weight
  )

### (Optional) collapse to an undirected edge --------------------------------

edges_undirected <- edges_recip %>% 
  transmute(
    u = pmin(caller, callee),      # canonical ordering
    v = pmax(caller, callee), 
    calls_ab, 
    calls_ba, 
    calls_total
  )

### Quick sanity report ------------------------------------------------------

recip_report <- tibble(
  directed_pairs_before  = nrow(edges_dir), 
  directed_pairs_after   = nrow(edges_recip),   # still directed (A->B)
  undirected_edges       = nrow(edges_undirected)
)

print(recip_report)

# Reciprocated network is in 'edges_undirected'; in 'edges_recip' direction is
# retained.

# cdr_internal -> full, asymmetric network
# edges_undirected -> strict reciprocated subset
