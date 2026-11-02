# =============================================================================
# 03  Network data preparation
#
# Filters the cleaned event-level data so that it represents personal
# communication between active users. Four steps:
#   1. Technical checks: missing values, invalid coordinates, duplicates.
#   2. Non-personal accounts: users at or above the 99.9th percentile in
#      outbound communication events, distinct contacts or communication
#      events per active day are removed, together with every communication
#      event they take part in.
#   3. Internal users: only communication events in which both sender and
#      receiver also send at least one communication event are kept, so that a
#      location is available for every user.
#   4. Tie reliability: a pair of users is kept if it has at least three
#      communication events in total or contact on at least two different
#      days.
#
# Input:  mob_data from script 02
# Output: edges_pass - the filtered event-level edge list used by all
#         following scripts
# =============================================================================

library(tidyverse)

# mob_data is taken from script 02 in the same R session.
# mob_data <- read_rds("path/to/cleaned_mob_data.rds")

# 1. Technical checks --------------------------------------------------------

## Missing values ------------------------------------------------------------

# Records without identifiers, time or coordinates cannot be used for edges
# or mobility measures.
cat("Rows before NA filter:", nrow(mob_data), "\n")

cdr_step1 <- mob_data |> 
  filter(
    !is.na(caller),
    !is.na(callee),
    !is.na(timestamp),
    !is.na(site_id),
    !is.na(lon), # Using original lon/lat for validation
    !is.na(lat)
  )

cat("Rows after NA filter:", nrow(cdr_step1), "\n")

## Valid coordinates ---------------------------------------------------------

# Coordinates must lie within the WGS84 range: -180 to 180 for longitude,
# -90 to 90 for latitude.

cdr_step2 <- cdr_step1 %>% 
  filter(
    between(longitude, -180, 180),
    between(latitude, -90, 90)
  )

## Duplicates ----------------------------------------------------------------

# Logging can produce exact duplicates. A duplicate is a record with the same
# sender, receiver and timestamp; only one copy is kept.
cdr_step3 <- cdr_step2 %>% 
  distinct(caller, callee, timestamp, .keep_all = TRUE)

## Summary after the technical checks ----------------------------------------

cdr_summary <- cdr_step3 |> 
  summarise(
    calls_left       = n(),
    unique_callers  = n_distinct(caller),
    unique_callees  = n_distinct(callee)
  )

print(cdr_summary)

# 2. Non-personal accounts ---------------------------------------------------

# Call centres, service numbers and machine-to-machine accounts show extreme,
# non-human activity. They are identified from their outbound activity and
# removed.

## Activity measures per sender ----------------------------------------------

caller_metrics <- function(df) {
  df %>% 
    mutate(date = as_date(timestamp)) %>% 
    group_by(caller) %>% 
    summarise(
      calls_out          = n(),                        # outbound communication events
      unique_callees     = n_distinct(callee),         # Breadth of contacts
      active_days        = n_distinct(date),           # Temporal span of activity
      calls_per_day      = calls_out / active_days,    # Normalised intensity
      .groups = "drop"
    )
}

# Activity before the filter
caller_stats_pre <- caller_metrics(cdr_step3)

## Thresholds ----------------------------------------------------------------

# A user is flagged when at or above the 99.9th percentile of any of the three
# activity measures. Percentile thresholds follow the data rather than fixed
# values.

# 99.9th percentile of each measure
thr_calls_out <- quantile(caller_stats_pre$calls_out,              0.999, na.rm = TRUE)
thr_unique_callees <- quantile(caller_stats_pre$unique_callees,    0.999, na.rm = TRUE)
thr_calls_per_day <- quantile(caller_stats_pre$calls_per_day,      0.999, na.rm = TRUE)

# Flag users at or above any of the three thresholds
suspect_users <- caller_stats_pre %>% 
  filter(
    calls_out >= thr_calls_out | 
      unique_callees >= thr_unique_callees | 
      calls_per_day >= thr_calls_per_day
  ) %>%  
  pull(caller)

## Remove flagged users ------------------------------------------------------

# Every communication event in which a flagged user is either sender or
# receiver is removed.
cdr_stage2 <- cdr_step3 %>% 
  filter(
    !(caller %in% suspect_users | callee %in% suspect_users)
  )

## Effect of the filter ------------------------------------------------------

# --- Summary ---
bot_report <- tibble(
  total_users_before        = n_distinct(c(cdr_step3$caller, cdr_step3$callee)),
  flagged_users             = length(suspect_users), 
  pct_users_flagged         = round(100 * flagged_users / total_users_before, 3), 
  rows_removed              = nrow(cdr_step3) - nrow(cdr_stage2), 
  threshold_calls_out       = thr_calls_out, 
  threshold_unique_callees  = thr_unique_callees,
  threshold_calls_per_day   = thr_calls_per_day
)

print(bot_report)

# --- Distributions before and after ---
caller_stats_post <- caller_metrics(cdr_stage2)   # after the filter

# Both stages in one table for plotting
caller_stats_long <- bind_rows(
  caller_stats_pre  %>% mutate(stage = "Before bot filter"), 
  caller_stats_post %>% mutate(stage = "After bot filter")
)

caller_stats_long$stage <- factor(caller_stats_long$stage, 
                                  levels = c("Before bot filter", "After bot filter"))

# Histogram of one measure, with its threshold as a dashed line
plot_metric <- function(var, cutoff, xlab) {
  ggplot(caller_stats_long, aes(.data[[var]])) +
    geom_histogram(bins = 100, fill = "grey70", colour = "grey40") +
    scale_x_log10() +
    geom_vline(xintercept = cutoff, 
               linetype = "dashed", 
               colour = "red", 
               linewidth = 0.7) +
    facet_wrap(~stage, nrow = 1) +
    labs(title = paste("Distribution of", xlab), 
         x = paste(xlab, "(log-scale)"), 
         y = "Count") +
    theme_minimal()
}

# Draw the plots
plot_calls_out <- plot_metric("calls_out", thr_calls_out, "Total Outbound Calls")
plot_unique_callees <- plot_metric("unique_callees", thr_unique_callees, "Distinct Outbound Contacts")
plot_calls_per_day <- plot_metric("calls_per_day", thr_calls_per_day, "Calls per Active Day")

# Display plots
plot_calls_out
plot_unique_callees
plot_calls_per_day

# 3. Internal users ----------------------------------------------------------

# Locations are recorded only for senders, so a user who never sends a
# communication event has no location. Only communication events between users
# who both send at least one are kept. This also removes numbers that only
# receive communication events, such as some service numbers and foreign
# roamers.

# --- Users who send at least one communication event ---
internal_users <- cdr_stage2 %>%
  distinct(caller) %>%
  pull(caller)

# --- Events with both endpoints in that set ---
cdr_internal <- cdr_stage2 %>% 
  filter(
    caller %in% internal_users, 
    callee %in% internal_users
  )

## Effect of the filter ------------------------------------------------------

internal_report <- tibble(
  rows_before   = nrow(cdr_stage2), 
  rows_after    = nrow(cdr_internal), 
  users_before  = n_distinct(c(cdr_stage2$caller, cdr_stage2$callee)),
  users_after   = n_distinct(c(cdr_internal$caller, cdr_internal$callee))
)

print(internal_report)

# 4. Tie reliability ---------------------------------------------------------

# Incidental contacts, such as wrong numbers or one-off service contacts, are
# removed.
# A tie is kept if it involves repeated interaction or contact on more than
# one day.

## Events and active days per pair -------------------------------------------

# Pairs are undirected here, so A -> B and B -> A count towards the same tie.
edges_und <- cdr_internal %>%
  mutate(
    call_day = as_date(timestamp),
    # Sorted pair: u is always the smaller ID
    u = pmin(caller, callee),
    v = pmax(caller, callee)
  ) %>%
  group_by(u, v) %>%
  summarise(
    calls_total   = n(),
    n_days        = n_distinct(call_day),
    .groups = "drop"
  )

## Choosing the threshold ----------------------------------------------------

# Many pairs interact only once or twice. The distribution of communication
# events per pair guides the threshold.
ggplot(edges_und, aes(calls_total)) +
  geom_histogram(bins = 100, fill = "grey75", colour = "grey35") +
  scale_x_log10(breaks = c(1, 2, 3, 4, 5, 10, 30, 100, 300, 1000)) +
  labs(
    title = "Distribution of Total Calls Per User Pair",
    x = "Calls per pair (log-scale)",
    y = "Number of pairs"
  ) +
  theme()

# Threshold: at least 3 communication events in total OR contact on at least 2
# different days.

## Apply the threshold -------------------------------------------------------

# The reliable pairs are found in the undirected summary, then used to keep
# the matching communication events in the event-level data.

# Reliable pairs
edge_thresh <- edges_und %>% 
  filter(calls_total  >= 3 | n_days >= 2)

# Events belonging to a reliable pair
edges_pass <- cdr_internal %>%
  mutate(
    u = pmin(caller, callee),
    v = pmax(caller, callee)
  ) %>%
  semi_join(edge_thresh, by = c("u", "v")) %>%
  select(-u, -v) # drop the pair helper columns

## Result --------------------------------------------------------------------

# edges_pass holds the filtered, directed communication events. It is the
# basis for the network (script 04) and the mobility measures (script 06).
cat("Final number of reliable call events:", nrow(edges_pass), "\n")
cat("Final number of unique users in the network:", n_distinct(c(edges_pass$caller, edges_pass$callee)), "\n")

# Optional: save to disk for use in a new session
# saveRDS(edges_pass, "data/final_edge_list.rds")
