# =============================================================================
# 11  Variable dictionary
#
# Readable labels for every variable in the analysis dataset, grouped by
# prefix:
#   sn_  social network measures (script 05)
#   sm_  spatial mobility measures (script 06)
#   cg_  settlement context measures (script 07)
# The dictionary is saved to disk, and a helper function adds the labels to
# any summary table that has a 'variable' column.
#
# Input:  none
# Output: var_labels, add_var_labels(), data/variable_dictionary.csv
# =============================================================================

# Dictionary -----------------------------------------------------------------

library(data.table)

var_labels <- fread("
                    variable, category, label
                    sn_indegree, Social Network Metrics (SN), Degree Centrality: In-Degree
                    sn_outdegree, Social Network Metrics (SN), Degree Centrality: Out-Degree
                    sn_totaldegree, Social Network Metrics (SN), Degree Centrality: All-Degree (Total)
                    sn_betweenness, Social Network Metrics (SN), Betweenness Centrality
                    sn_closeness, Social Network Metrics (SN), Closeness Centrality
                    sn_clustering, Social Network Metrics (SN), Clustering Coefficient
                    sn_reciprocity, Social Network Metrics (SN), Reciprocity Index
                    sm_avg_daily_rog, Spatial Mobility Metrics (SM), Radius of Gyration (Average Daily)
                    sm_avg_daily_nul, Spatial Mobility Metrics (SM), Number of Unique Locations (Average Daily)
                    sm_avg_daily_entr, Spatial Mobility Metrics (SM), Spatial Entropy (Average Daily)
                    sm_avg_daily_moves, Spatial Mobility Metrics (SM), Movement Frequency (Average Daily)
                    sm_avg_workday_rog, Spatial Mobility Metrics (SM), Radius of Gyration (Average Workday)
                    sm_avg_workday_nul, Spatial Mobility Metrics (SM), Number of Unique Locations (Average Workday)
                    sm_avg_workday_entr, Spatial Mobility Metrics (SM), Spatial Entropy (Average Workday)
                    sm_avg_workday_moves, Spatial Mobility Metrics (SM), Movement Frequency (Average Workday)
                    sm_avg_weekend_rog, Spatial Mobility Metrics (SM), Radius of Gyration (Average Weekend)
                    sm_avg_weekend_nul, Spatial Mobility Metrics (SM), Number of Unique Locations (Average Weekend)
                    sm_avg_weekend_entr, Spatial Mobility Metrics (SM), Spatial Entropy (Average Weekend)
                    sm_avg_weekend_moves, Spatial Mobility Metrics (SM), Movement Frequency (Average Weekend)
                    sm_rog_ratio, Spatial Mobility Metrics (SM), Radius of Gyration (Ratio: Workday vs. Weekend)
                    sm_nul_ratio, Spatial Mobility Metrics (SM), Number of Unique Locations (Ratio: Workday vs. Weekend)
                    sm_entr_ratio, Spatial Mobility Metrics (SM), Spatial Entropy (Ratio: Workday vs. Weekend)
                    sm_moves_ratio, Spatial Mobility Metrics (SM), Movement Frequency (Ratio: Workday vs. Weekend)
                    sm_rog_total, Spatial Mobility Metrics (SM), Radius of Gyration (Total)
                    sm_nul_total, Spatial Mobility Metrics (SM), Number of Unique Locations (Total)
                    sm_entr_total, Spatial Mobility Metrics (SM), Spatial Entropy (Total)
                    sm_active_days_total, Spatial Mobility Metrics (SM), Number of Active Days (Total)
                    cg_avg_pct_big_city, Contextual Variables (CG), Daily Activity: Big City (Average Daily)
                    cg_avg_pct_small_city, Contextual Variables (CG), Daily Activity: Small City (Average Daily)
                    cg_avg_pct_rural, Contextual Variables (CG), Daily Activity: Rural (Average Daily)
                    cg_domset, Contextual Variables (CG), Dominant Settlement Type
                    cg_geodiv_hierarchy, Contextual Variables (CG), Diversity of Visited Settlement Hierarchy Type
                    cg_geodiv_municipality, Contextual Variables (CG), Diversity of Visited Municipalities
                    cg_pct_big_city, Contextual Variables (CG), Big City (Percentage of Calls)
                    cg_pct_small_city, Contextual Variables (CG), Small City (Percentage of Calls)
                    cg_pct_rural, Contextual Variables (CG), Rural (Percentage of Calls)
                    ")

## Save to disk --------------------------------------------------------------

fwrite(var_labels, "data/variable_dictionary.csv")

## Helper --------------------------------------------------------------------

# Row number, used to keep tables in dictionary order
var_labels[, label_order := .I]   # .I row number inside data.table

add_var_labels <- function(dt, var_col = "variable") {
  lbl <- var_labels   # fread("data/variable_dictionary.csv")
  merge(dt, lbl, by.x = var_col, by.y = "variable", all.x = TRUE, sort = FALSE) %>%
    relocate(category, .after = variable) %>%
    relocate(label, .after = category)
}

# gl <- function() fread("data/variable_dictionary.csv") # reload from disk

## Examples: adding labels to a summary table --------------------------------

# Three equivalent ways. num_stats is the summary table created in the
# exploratory analysis (script 12).

# With the helper
num_stats <- add_var_labels(num_stats)   # uses helper above
data.table::setorder(num_stats, label_order)

# With merge()
num_stats <- merge(num_stats, var_labels, by = "variable", all.x = TRUE) %>%
  relocate(category, .after = variable) %>%
  relocate(label, .after = category)
data.table::setorder(num_stats, label_order)

# With a join, keeping dictionary order
var_labels <- var_labels %>%
  mutate(label_order = row_number())  # 1, 2, 3, ...

num_stats <- var_labels %>%
  left_join(num_stats, by = "variable") %>%
  arrange(label_order) %>%    # enforce dictionary order
  select(-label_order)        # drop helper column
