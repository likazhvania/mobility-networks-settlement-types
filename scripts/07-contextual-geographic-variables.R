# =============================================================================
# 07  Settlement context per user
#
# Describes where each user's activity takes place, using the settlement
# types from script 01:
#   - dominant settlement type (cg_domset): the type from which the user sends
#     most of their communication events - the grouping used throughout the
#     study
#   - number of distinct settlement types and municipalities visited
#   - share of the user's communication events sent from big cities, small
#     cities and rural settlements
#
# Input:  edges_pass from script 03
# Output: user_contextual_metrics - one row per user
# =============================================================================

library(tidyverse)

# edges_pass is taken from script 03 in the same R session; to start from a
# saved copy instead:
# edges_pass <- readRDS("data/final_edge_list.rds")

# Dominant settlement type ---------------------------------------------------

# Most frequent value in a vector
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

user_contextual_metrics_part1 <- edges_pass %>%
  group_by(caller) %>%
  summarise(
    cg_domset = get_mode(settlement_hierarchy),
    .groups = "drop"
  )

# Diversity of settings visited ----------------------------------------------

# How many different settlement types and municipalities a user's
# communication events come from.

user_contextual_metrics_part2 <- edges_pass %>%
  group_by(caller) %>%
  summarise(
    # Settlement types
    cg_geodiv_hierarchy = n_distinct(settlement_hierarchy, na.rm = TRUE),

    # Municipalities
    cg_geodiv_municipality = n_distinct(municipality, na.rm = TRUE),

    .groups = "drop"
  )

# Share of activity by settlement type ---------------------------------------

# Share of the user's communication events sent from each of the three
# settlement types.

user_contextual_metrics_part3 <- edges_pass %>%
  group_by(caller) %>%
  summarise(

    cg_pct_big_city = mean(settlement_hierarchy == "Urban: big city", na.rm = TRUE),
    cg_pct_small_city = mean(settlement_hierarchy == "Urban: small city", na.rm = TRUE),
    cg_pct_rural = mean(settlement_hierarchy == "rural", na.rm = TRUE),

    .groups = "drop"
  )

# Combine --------------------------------------------------------------------

cg_metrics_list <- list(
  user_contextual_metrics_part1,
  user_contextual_metrics_part2,
  user_contextual_metrics_part3
)

# Join by user
user_contextual_metrics <- cg_metrics_list %>%
  reduce(full_join, by = "caller") %>%
  # Same key name as the other user tables
  rename(user_id = caller)

# --- Check ---
cat("Dimensions of the final user contextual metrics dataset:", dim(user_contextual_metrics), "\n")
glimpse(user_contextual_metrics)

# Optional: save for a new session -------------------------------------------

# saveRDS(user_contextual_metrics, "data/user_contextual_metrics.rds")
