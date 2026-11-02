# =============================================================================
# 06  Spatial mobility measures
#
# Calculates the mobility measures for every user from the locations of the
# communication events they send. The measures used in the study are the
# totals over the whole month:
#   - radius of gyration: root-mean-square distance of the user's distinct
#     visited locations from their centroid; each location counts once,
#     however many communication events were recorded there
#   - unique locations: number of distinct cell tower sites
#   - spatial entropy: Shannon entropy (base 2) of the user's communication
#     events across sites; higher values mean more varied locations
#   - active days: number of days with at least one communication event
#
# Daily, workday and weekend averages and their ratios are also calculated.
# They were prepared for an earlier profiling design and are not used in the
# reported analysis.
#
# Input:  edges_pass from script 03
# Output: user_mobility_metrics - one row per user
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # For easier handling of dates and weekdays.

# edges_pass is taken from script 03 in the same R session; to start from a
# saved copy instead:
# edges_pass <- readRDS("data/final_edge_list.rds")

# Date, weekday and workday/weekend columns, added to edges_pass itself.
edges_pass <- edges_pass %>%
  mutate(
    date = as_date(timestamp),
    day_of_week = wday(date, label = TRUE, week_start = 1), # Mon=1, Sun=7
    week_type = if_else(day_of_week %in% c("Sat", "Sun"), "weekend", "workday")
  )

# Daily measures -------------------------------------------------------------

# One row per user and active day. Slow on the full data.
daily_mobility_summary <- edges_pass %>%
  # One group per user and day
  group_by(caller, date, week_type) %>%
  summarise(
    # Daily Number of Unique Locations
    nul_daily = n_distinct(site_id),

    # Daily moves: distinct sites minus one
    moves_daily = n_distinct(site_id) - 1,

    # Daily Radius of Gyration
    rog_daily = {
      unique_locs <- distinct(tibble(x = x_coord, y = y_coord))
      if (nrow(unique_locs) > 1) {
        center_mass_x <- mean(unique_locs$x)
        center_mass_y <- mean(unique_locs$y)
        sqrt(mean((unique_locs$x - center_mass_x)^2 + (unique_locs$y - center_mass_y)^2))
      } else {
        0 # 0 when only one location is visited
      }
    },

    # Daily Spatial Entropy
    entr_daily = {
      if (n_distinct(site_id) > 1) {
        freq_table <- table(site_id)
        probabilities <- freq_table / sum(freq_table)
        -sum(probabilities * log2(probabilities))
      } else {
        0 # 0 when only one location is visited
      }
    },

    # Daily share of communication events in each settlement type
    pct_big_city_daily = mean(settlement_hierarchy == "Urban: big city", na.rm = TRUE), 
    pct_small_city_daily = mean(settlement_hierarchy == "Urban: small city", na.rm = TRUE),
    pct_rural_daily = mean(settlement_hierarchy == "rural", na.rm = TRUE),

    .groups = "drop"
  )

# moves_daily cannot be negative
daily_mobility_summary <- daily_mobility_summary %>%
  mutate(moves_daily = if_else(moves_daily < 0, 0, moves_daily))

glimpse(daily_mobility_summary)

# Daily, workday and weekend averages ----------------------------------------

# Averages of the daily measures per user. Prepared for an earlier profiling
# design; not used in the reported analysis.

user_granular_metrics <- daily_mobility_summary %>%
  group_by(caller) %>%
  summarise(

  # --- Average day ---
  sm_avg_daily_rog = mean(rog_daily, na.rm = TRUE),
  sm_avg_daily_nul = mean(nul_daily, na.rm = TRUE),
  sm_avg_daily_entr = mean(entr_daily, na.rm = TRUE),
  sm_avg_daily_moves = mean(moves_daily, na.rm = TRUE),

  # --- Average workday ---
  sm_avg_workday_rog = mean(rog_daily[week_type == "workday"], na.rm = TRUE),
  sm_avg_workday_nul = mean(nul_daily[week_type == "workday"], na.rm = TRUE),
  sm_avg_workday_entr = mean(entr_daily[week_type == "workday"], na.rm = TRUE),
  sm_avg_workday_moves = mean(moves_daily[week_type == "workday"], na.rm = TRUE),

  # --- Average weekend day ---
  sm_avg_weekend_rog = mean(rog_daily[week_type == "weekend"], na.rm = TRUE),
  sm_avg_weekend_nul = mean(nul_daily[week_type == "weekend"], na.rm = TRUE),
  sm_avg_weekend_entr = mean(entr_daily[week_type == "weekend"], na.rm = TRUE),
  sm_avg_weekend_moves = mean(moves_daily[week_type == "weekend"], na.rm = TRUE),

  # --- Average daily share of communication events by settlement type ---
  cg_avg_pct_big_city = mean(pct_big_city_daily, na.rm = TRUE),
  cg_avg_pct_small_city = mean(pct_small_city_daily, na.rm = TRUE),
  cg_avg_pct_rural = mean(pct_rural_daily, na.rm = TRUE),

  .groups = "drop"
  ) %>%
  # --- Workday / weekend ratios ---
  mutate(
    sm_rog_ratio = sm_avg_workday_rog / sm_avg_weekend_rog,
    sm_nul_ratio = sm_avg_workday_nul / sm_avg_weekend_nul,
    sm_entr_ratio = sm_avg_workday_entr / sm_avg_weekend_entr,
    sm_moves_ratio = sm_avg_workday_moves / sm_avg_weekend_moves
  ) %>%
  # Ratios are undefined when the weekend value is 0
  mutate(across(ends_with("_ratio"), ~if_else(is.finite(.), ., NA_real_))) %>%
  # Rename for consistency
  rename(user_id = caller)

glimpse(user_granular_metrics)

# Monthly totals -------------------------------------------------------------

# The measures used in the study, calculated over the whole month directly
# from the communication events.

user_total_metrics <- edges_pass %>%
  group_by(caller) %>% 
  summarise(
    # Total Number of Unique Locations
    sm_nul_total = n_distinct(site_id),

    # Total Active Days
    sm_active_days_total = n_distinct(date),

    # Total Spatial Entropy
    sm_entr_total = {
      freq_table <- table(site_id)
      probabilities <- freq_table / sum(freq_table)
      -sum(probabilities * log2(probabilities))
    },

    # Total Radius of Gyration
    sm_rog_total = {
      unique_locs <- distinct(tibble(x = x_coord, y = y_coord))
      if (nrow(unique_locs) > 1) {
        center_mass_x <- mean(unique_locs$x)
        center_mass_y <- mean(unique_locs$y)
        sqrt(mean((unique_locs$x - center_mass_x)^2 + (unique_locs$y - center_mass_y)^2))
      } else { 0 }
    },

    .groups = "drop"
  ) %>%
  # Rename for consistency
  rename(user_id = caller)

# Combine --------------------------------------------------------------------

user_mobility_metrics <- full_join(
  user_granular_metrics,
  user_total_metrics,
  by = "user_id"
) %>%
  # Rename for consistency
  #rename(user_id = caller) %>%
  # Ratios are undefined when the weekend value is 0
  mutate(across(ends_with("_ratio"), ~if_else(is.finite(.), ., NA_real_)))

# --- Check ---
cat("Dimensions of the final user mobility metrics dataset:", dim(user_mobility_metrics), "\n")
glimpse(user_mobility_metrics)

# Optional: save for a new session -------------------------------------------

# saveRDS(user_mobility_metrics, "data/user_mobility_metrics.rds")
