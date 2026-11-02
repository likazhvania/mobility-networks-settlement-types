# =============================================================================
# Development script - not part of the final analysis
#
# Technical checks on the event data; earlier version of step 1 of script 03.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# - Technical sanity checks (catch obvious garbage)
#   Check | Why it matters | Typical rule of thumb
#   Non-null IDs | avoid phantom nodes/edges | caller != NULL, callee != NULL,
#   cell_id != NULL, site_id != NULL
#   Valid coordinates | allows distance & mobility metrics | drop rows where
#   lon/lat (x/y) outside [-180,180]/[-90,90]
#   Duplicate rows | billing duplicates or re-transmitted CDRs | drop exact
#   duplicates on (caller, callee, timestamp)

library(tidyverse)

## Drop rows with NULL/NA IDs (caller, callee, site_id, lon, lat) ------------

# filter(!is.na(...)) removes phantom rows with missing values

cdr_step1 <- mob_data |> 
  filter(
    !is.na(caller),
    !is.na(callee),
    !is.na(timestamp),
    !is.na(site_id),
    !is.na(lon),
    !is.na(lat)
  )

## Remove invalid coordinates ------------------------------------------------

# between(lon, ...) ensures coordinates are on planet Earth so mobility metrics won't explode

cdr_step2 <- cdr_step1 |> 
  filter(
    between(lon, -180, 180),
    between(lat, -90, 90)
  )

## Drop exact duplicate CDR rows (caller, callee, timestamp) -----------------

# distinct(caller, callee, timestamp) collapses duplicates billing entries (same pair & start time)

cdr_step3 <- cdr_step2 |> 
  distinct(caller, callee, timestamp, .keep_all = TRUE)

## Quick sanity summary ------------------------------------------------------

# Gives a quicj count to see how many rows survived the gauntlet
cdr_summary <- cdr_step3 |> 
  summarise(
    calls_left       = n(),
    unique_callers  = n_distinct(caller),
    unique_callees  = n_distinct(callee)
  )

print(cdr_summary)
