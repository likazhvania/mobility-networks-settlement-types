# =============================================================================
# Development script - not part of the final analysis
#
# Filter for non-personal accounts; earlier version of step 2 of script 03.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# The logic is:
# 1. Caller-level metrics
# - calls_out = total calls places
# - unique_callees = number of distinct SIMs reached
# - active_days = on how many distinct days the caller was active
# - calls_per_day = intensity normalised by activity span

# 2. Automatic thresholding
# - Flag any caller that lands in the top 0.1% (99.9th percentile) on either
#   calls_out or unique_callees.
#   (Tweak the percentile after glancing at the histogram)

# Remove every CDR involving a flagged SIM
# (both directions, so the number disappears as caller and as callee)

## Why these metrics? --------------------------------------------------------

# | Metric             | Rationale                                                                                                                           | Typical bot pattern  |
# |-----------------|---------------------------------------|-----------------|
# | calls_out      | Sheer volume; tele-marketers fire thousands of outbound calls.                                                                      | Extremely high total |
# | unique_callees | Service numbers reach *many* SIMs once each.                                                                                        | Long right-tail      |
# | calls_per_day  | Sustained daily intensity. Distinguishes a power-user (even a popular teenager) from an auto-dialler that calls non-stop every day. | \>100 calls/day      |

library(tidyverse)

## Build caller-level activity metrics function ------------------------------

caller_metrics <- function(df) {
  df |> 
    mutate(
      date = as_date(timestamp)
    ) |> 
    group_by(caller) |> 
    summarise(
      calls_out          = n(),                        # total calls place
      unique_callees     = n_distinct(callee),         # breadth of contacts
      active_days        = n_distinct(date),           # temporal span
      calls_per_day      = calls_out / active_days,    # normalised intensity
      .groups = "drop"
    )
}

caller_stats_pre <- caller_metrics(cdr_step3)     # *before* bot filter
#caller_stats_post <- caller_metrics(cdr_stage2)   # *after* bot filter

## Choose automatic cut-offs -------------------------------------------------

# We take the 99.9th percentiles; adjust if too strict/lenient
thr_calls_out <- quantile(caller_stats_pre$calls_out,              0.999, na.rm = TRUE)
thr_unique_callees <- quantile(caller_stats_pre$unique_callees,    0.999, na.rm = TRUE)
thr_calls_per_day <- quantile(caller_stats_pre$calls_per_day,      0.999, na.rm = TRUE)

## Flag suspects (ANY of the three triggers) ---------------------------------

suspect_users <- caller_stats_pre |> 
  filter(calls_out >= thr_calls_out | 
           unique_callees >= thr_unique_callees | 
           calls_per_day >= thr_calls_per_day) |> 
  pull(caller)

## Remove every edge that involves a suspect ---------------------------------

cdr_stage2 <- cdr_step3 |> 
  filter(
    !(caller %in% suspect_users | callee %in% suspect_users)
  )

## Quick sanity report -------------------------------------------------------

bot_report <- tibble(
  total_sims_before   = n_distinct(cdr_step3$caller) + 
    n_distinct(cdr_step3$callee), 
  flagged_sims        = length(suspect_users), 
  pct_flagged         = round(100 * flagged_sims / total_sims_before, 3), 
  rows_removed        = nrow(cdr_step3) - nrow(cdr_stage2), 
  thr_calls_out       = thr_calls_out, 
  thr_unique_callees  = thr_unique_callees,
  thr_calls_per_day   = thr_calls_per_day
)

print(bot_report)

## Caller-level activity metrics after bot filtering -------------------------

#caller_stats_pre <- caller_metrics(cdr_step3)     # *before* bot filter
caller_stats_post <- caller_metrics(cdr_stage2)   # *after* bot filter

## Plot helper: returns a patchwork of before / after histograms -------------

# Bind the before and after caller-level tables into one data frame, add a
# stage flag, and draw each metric with facet_wrap(\~stage) so the two
# histograms appear side-by-side in the same figure.

### Bind rows ----------------------------------------------------------------

caller_stats_long <- bind_rows(
  caller_stats_pre  %>% mutate(stage = "Before bot filter"), 
  caller_stats_post %>% mutate(stage = "After bot filter")
)

caller_stats_long$stage <- factor(caller_stats_long$stage, 
                                  levels = c("Before bot filter", "After bot filter"))

### Helper for one-liner plotting --------------------------------------------

plot_metric <- function(var, cutoff, xlab) {
  ggplot(caller_stats_long, aes(.data[[var]])) + 
    geom_histogram(bins = 100, fill = "grey70", colour = "grey40") + 
    scale_x_log10() + 
    geom_vline(xintercept = cutoff, 
               linetype = "dashed", 
               colour = "red", 
               linewidth = 0.7) + 
    facet_wrap(~stage, nrow = 1) +   # side-by-sude panels
    labs(title = xlab, x = xlab, y = "Count")
}

## Draw the three figures (they will follow one another) ---------------------

plot_calls_out <- plot_metric("calls_out", 
                              thr_calls_out, 
                              "Total outbound calls (log-scale)")

plot_unique_callees <- plot_metric("unique_callees", 
                                   thr_unique_callees, 
                                   "Distinct callees (log-scale)")

plot_calls_per_day <- plot_metric("calls_per_day", 
                                  thr_calls_per_day, 
                                  "Calls per activity day (log-scale)")

plot_calls_out
plot_unique_callees
plot_calls_per_day
