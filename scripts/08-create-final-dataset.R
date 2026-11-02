# =============================================================================
# 08  Final analysis dataset
#
# Joins the network, mobility and settlement-context measures into one table
# with one row per user. Network measures exist for every user in the graph,
# but mobility and settlement measures only for users who send at least one
# communication event, because locations are recorded for senders only. The
# analysis is therefore limited to users with all three sets of measures
# (inner join). Users dropped by the join are compared with those kept, as a
# check.
#
# Input:  user_network_metrics (script 05), user_mobility_metrics (06),
#         user_contextual_metrics (07)
# Output: analysis_data - the user-level dataset used in all analyses
# =============================================================================

library(tidyverse)

# The three tables are taken from scripts 05-07 in the same R session; to
# start from saved copies instead:
# user_network_metrics <- readRDS("data/user_network_metrics.rds")
# user_mobility_metrics <- readRDS("data/user_mobility_metrics.rds")
# user_contextual_metrics <- readRDS("data/user_contextual_metrics.rds")

# Number of users in each table ----------------------------------------------

# The network table is expected to contain all users of the other two.

cat("Number of users with Social Network metrics:", nrow(user_network_metrics), "\n")
cat("Number of users with Spatial Mobility metrics:", nrow(user_mobility_metrics), "\n")
cat("Number of users with Contextual & Geographic metrics:", nrow(user_contextual_metrics))

# Users without mobility data ------------------------------------------------

# Users who only appear as receivers in the filtered data have no location and
# therefore no mobility measures. A left join shows how they compare with the
# users who are kept.

# --- Same type for the user key ---
# user_id is character in the network table (from igraph) and integer in the
# mobility and settlement tables, so both are converted to character.

user_mobility_metrics <- user_mobility_metrics %>%
  mutate(user_id = as.character(user_id))

user_contextual_metrics <- user_contextual_metrics %>%
  mutate(user_id = as.character(user_id))

# All network users, with mobility measures where available
diagnostic_join <- left_join(user_network_metrics, user_mobility_metrics, by = "user_id")

# Users with mobility measures: kept
included_users <- diagnostic_join %>%
  filter(!is.na(sm_rog_total)) # missing when no mobility data

# Users without mobility measures: dropped
excluded_users <- diagnostic_join %>%
  filter(is.na(sm_rog_total))

cat("Number of users to be INCLUDED in final analysis:", nrow(included_users), "\n")
cat("Number of users to be EXCLUDED from final analysis:", nrow(excluded_users), "\n")

# --- Network measures of the two groups ---
cat("\n--- Summary of Network Metrics for INCLUDED Users ---\n")
summary(included_users %>% select(starts_with("sn_")))

cat("\n--- Summary of Network Metrics for EXCLUDED User ---\n")
summary(excluded_users %>% select(starts_with("sn_")))

# Systematic differences here - for example a much higher in-degree among the
# dropped users - would mean that the analysis leaves out users who mostly
# receive contacts.

# Final dataset --------------------------------------------------------------

# Only users present in all three tables are kept.
metrics_to_join <- list(
  user_network_metrics,
  user_mobility_metrics,
  user_contextual_metrics
)

# Inner join of the three tables by user_id
analysis_data <- metrics_to_join %>%
  reduce(inner_join, by = "user_id")

# --- Final sample size ---
cat("Dimensions of the final merged analysis dataset:", dim(analysis_data), "\n")
cat("The number of rows now represents the final sample size for our study. \n")

# --- Structure ---

glimpse(analysis_data)

# Optional: save for a new session -------------------------------------------

# saveRDS(analysis_data, "data/analysis_data.rds")
