# =============================================================================
# Development script - not part of the final analysis
#
# Mobility measures tested on a small random sample; earlier version of
# script 06.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

library(tidyverse)
library(geosphere)

# Temporary sample data

sample_cdr <- sample_n(cdr_filtered, 10000)

mobility_tbl_1 <- sample_cdr %>% 
  group_by(caller) %>% 
  summarise(
    n_calls         = n(), 
    n_sites         = n_distinct(site_id), 
    rog             = {
      pts <- cbind(lon, lat)
      sqrt(mean(distHaversine(pts, colMeans(pts))^2))
    },
    tower_entropy   = {p <- prop.table(table(site_id)); -sum(p*log2(p))},
    home_ratio      = {
      top <- max(table(site_id)); top / n_calls
    }, 
    .groups = "drop"
  )

mobility_tbl <- sample_cdr %>% 
  arrange(caller, timestamp) %>%    # 1 chronological within user
  group_by(caller) %>% 
  summarise(
    # ------ span ------
    n_calls                 = n(), 
    n_sites                 = n_distinct(site_id), 
    rog_km                  = {
      pts <- cbind(lon, lat)
      sqrt(mean(distHaversine(pts, colMeans(pts))^2)) / 1000 # km
    },
    # ------ diversity of physical sites ------
    site_entropy            = {
      p <- prop.table(table(site_id)); -sum(p*log2(p))
    }, 
    home_ratio              = {
      top <- max(table(site_id)); top / n_calls
    }, 
    # ------ settlement-level diversity ------
    n_settlements           = n_distinct(settlement),
    settle_entropy          = {
      p <- prop.table(table(settlement)); -sum(p*log2(p))
    }, 
    # share of calls made outside the dominant (home) settlement
    calls_outside_home      = {
      home <- names(sort(table(settlement), decreasing=TRUE))[1]
      mean(settlement != home)
    },
    # ------ urban / rural mix ------
    n_settle_types          = n_distinct(settlement_type),     # 1 or 2
    urban_share             = mean(settlement_type == "urban"),
    urb_rur_entropy         = {
      p <- prop.table(table(settlement_type)); -sum(p*log2(p))
    },
    # ------ cross-boundary transitions (optional, heavier) ------  fixed cross-boundary counter
    cross_settle_moves      = sum(settlement != lag(settlement, default = first(settlement))),

    .groups = "drop"
  )
