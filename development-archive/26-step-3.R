# =============================================================================
# Development script - not part of the final analysis
#
# Earlier version of the contact-composition part of the individual-level
# analysis (script 14). Incomplete: the file does not run as a whole.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(purrr)
library(broom)
library(scales)

# Preparing data -------------------------------------------------------------

## Inputs --------------------------------------------------------------------

nodes <- analysis_data %>%
  select(user_id, cg_domset) %>% distinct()

# Map long labels to class codes

map_dom <- c("Urban: big city"="B", "Urban: small city"="S", "rural"="R")
nodes <- nodes %>% mutate(type = recode(cg_domset, !!!map_dom))

## Standardise edge endpoints (directed or undirected) -----------------------

# We use undirected edges/ties
edges_std <- undirected_edges_weighted

edges_std$u <- as.character(edges_std$u)
edges_std$v <- as.character(edges_std$v)

# If ties are undirected but appear once per dyad, create both directions
edges_long <- bind_rows(
  edges_std %>% transmute(ego = u, alter = v, across(-c(u,v), identity)),
  edges_std %>% transmute(ego = v, alter = u, across(-c(u,v), identity))
)

## Attach types and build ego-alter mix --------------------------------------

ego_alter <- edges_long %>%
  left_join(nodes %>% select(user_id, type) %>% rename(ego_type = type),
            by = c("ego" = "user_id")) %>%
  left_join(nodes %>% select(user_id, type) %>% rename(alter_type = type),
            by = c("alter" = "user_id")) %>%
  filter(!is.na(ego_type), !is.na(alter_type))

# Weighted shares (number of calls)

wt <- "weight"
if (!wt %in% names(ego_alter)) ego_alter[[wt]] <- 1

# Compute per-ego shares:

per_ego <- ego_alter %>%
  group_by(ego, ego_type) %>%
  summarise(
    n_alters = n_distinct(alter),
    w_sum    = sum(.data[[wt]], na.rm = TRUE),
    own_w    = sum(.data[[wt]] * (alter_type == ego_type), na.rm = TRUE),
    toB_w    = sum(.data[[wt]] * (alter_type == "B"), na.rm = TRUE),
    toS_w    = sum(.data[[wt]] * (alter_type == "S"), na.rm = TRUE),
    toR_w    = sum(.data[[wt]] * (alter_type == "R"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(w_sum > 0) %>%
  mutate(
    own_share        = own_w / w_sum,
    cross_sett_share = 1 - own_share,
    to_big_share     = toB_w / w_sum,
    to_small_share   = toS_w / w_sum,
    to_rural_share   = toR_w / w_sum
  )

## Join ego SN/SM metrics (for Step 3 correlations) --------------------------

ego_df <- per_ego %>%
  left_join(
    analysis_data %>%
      select(user_id,
             sm_rog_total, sm_nul_total, sm_entr_total, sm_active_days_total,
             sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity),
    by = c("ego" = "user_id")
  )

ego_df <- ego_df %>%
  dplyr::mutate(
    dplyr::across(
      c(cross_sett_share, sm_rog_total, sm_entr_total, sm_nul_total, sm_active_days_total,
        sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity),
      ~ rank(., ties.method = "average"),
      .names = "{.col}_r"
    )
  )

# Spearman (overall + by ego type), with NUL option --------------------------

# Spearman correlations (mobility) -------------------------------------------

spearman_one <- function(df, x, y) {
  broom::tidy(cor.test(df[[x]], df[[y]], method = "spearman", exact = FALSE))
}

do_spearman <- function(df, y) {
  dplyr::bind_rows(
    spearman_one(df, "cross_sett_share", y) %>% dplyr::mutate(stratum="Overall"),
    df %>% dplyr::group_by(ego_type) %>%
      dplyr::group_modify(~spearman_one(.x, "cross_sett_share", y)) %>%
      dplyr::rename(stratum = ego_type)
  ) %>% dplyr::mutate(metric = y)
}

tab_mob <- dplyr::bind_rows(
  do_spearman(ego_df, "sm_rog_total"),
  do_spearman(ego_df, "sm_entr_total")
  # Optional: can be included NUL in appendix
  # do_spearman(ego_df, "sm_nul_total"))
) %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(p_adj = p.adjust(p.value, method="BH")) %>%
  dplyr::ungroup()

tab_mob

# Rank-partial correlations (network) (control for active days + degree) -----

# --- helper: partial correlation via residualization on RANKS ---
pcor_rank_resid <- function(df, x, y, controls) {
  # keep only needed columns
  sub   <- df[, c(x, y, controls)]
  sub   <- sub[stats::complete.cases(sub), , drop = FALSE]
  n     <- nrow(sub)
  if (n < 3L) return(tibble(estimate = NA_real_, p.value = NA_real_, n = n))

  # rank-transform (Spearman)
  sub_r <- as.data.frame(lapply(sub, function(v) rank(v, ties.method = "average")))

  # residualize x and y on controls (if controls exist)
  if (length(controls) > 0) {
    rx <- resid(lm(sub_r[[x]] ~ ., data = sub_r[, controls, drop = FALSE]))
    ry <- resid(lm(sub_r[[y]] ~ ., data = sub_r[, controls, drop = FALSE]))
  } else {
    rx <- sub_r[[x]]
    ry <- sub_r[[y]]
  }

  # Pearson on residuals = partial Spearman
  ct <- suppressWarnings(cor.test(rx, ry, method = "pearson"))
  tibble(estimate = unname(ct$estimate),
         p.value  = unname(ct$p.value),
         n = n)
}

# --- run overall + by ego_type, then BH adjust ---
do_pcor <- function(df, target, controls, label) {
  bind_rows(
    pcor_rank_resid(df, "cross_sett_share", target, controls) %>% mutate(stratum="Overall"),
    df %>% group_by(ego_type) %>%
      group_modify(~ pcor_rank_resid(.x, "cross_sett_share", target, controls)) %>%
      rename(stratum = ego_type)
  ) %>% mutate(metric = label)
}

# --- choose controls (keep the model compact) ---
controls <- c("sm_active_days_total", "sn_totaldegree")   # or swap totaldegree for indegree/outdegree

# --- build the table for SN outcomes ---
tab_sn <- bind_rows(
  do_pcor(ego_df, "sn_betweenness", controls, "betweenness"),
  do_pcor(ego_df, "sn_clustering",  controls, "clustering"),
  do_pcor(ego_df, "sn_indegree",    controls, "indegree"),
  do_pcor(ego_df, "sn_outdegree",   controls, "outdegree")
) %>%
  group_by(metric) %>%
  mutate(p_adj = p.adjust(p.value, method="BH")) %>%
  ungroup()

tab_sn

# Check runs

# All shares finite and in [0,1]?
stopifnot(all(per_ego$own_share >= 0 & per_ego$own_share <= 1, na.rm = TRUE))
stopifnot(all(per_ego$cross_sett_share >= 0 & per_ego$cross_sett_share <= 1, na.rm = TRUE))

# Do shares sum to 1 (witin numeric tolerance)?
sums <- with(per_ego, own_share + to_big_share + to_small_share + to_rural_share)
summary(abs(sums - 1))

# If the last line shows only tiny (nearly 1e-16) deviations, that's great.

tab_mob

tab_sn

# Visuals --------------------------------------------------------------------

## Heatmap of correlations (overall + by ego type) ---------------------------

# Spearman correlations: tab_mob (mobility) and tab_sn (network)

viz_corr <- function(df, title){
  ggplot(df, aes(x = metric, y = stratum, fill = estimate)) +
    geom_tile() +
    geom_text(aes(label = scales::number(estimate, accuracy = .01))) +
    scale_fill_gradient2(limits = c(-.5, .5), oob = scales::squish) +
    labs(x = NULL, y = NULL, fill = "p", title = title) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

p1 <- viz_corr(tab_mob, "Spearman p: cross_sett_share vs mobility")
p2 <- viz_corr(tab_sn, "Partial p: cross_sett_share vs network (| active days, degree)")
p1; p2

# Shades sgow effect size, numbers show p. Rows are overall, B, S, R.

## Box/line by quintiles (reader-friendly) -----------------------------------

ego_df <- ego_df %>%
  mutate(q = cut(cross_sett_share, quantile(cross_sett_share, probs = 0:5/5, na.rm = TRUE),
                 include.lowest = TRUE, labels = paste0("Q", 1:5)))

ggplot(ego_df, aes(q, sm_rog_total)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = median, geom = "point", size = 1.8) +
  facet_wrap(~ ego_type, nrow = 1) +
  labs(x = "Cross-settlement share (quantiles)", y = "Radius of gyration",
       title = "RoG rises with cross-settlement share, within each ego type") +
  theme_minimal(base_size = 11)

glimpse(ego_df)

# 0) A small catalogue of metrics to iterate
metrics <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", 
             "sn_betweenness", "sn_closeness", "sn_clustering", "sn_reciprocity",
             "sm_rog_total", "sm_nul_total", "sm_entr_total", 'sm_active_days_total')

# 1) Binned line (median with IQR ribbons) - shape & tails, by ego_type
viz_binned <- function(df, metric, nbins = 30) {
  # robust: fixed breakpoints, so mutate never looks for a column called 'nbins'
  brks <- seq(0, 1, length.out = nbins + 1)  # assumes cross_sett_share [0,1]

  df %>%
    mutate(bin = cut(cross_sett_share, breaks = brks, include.lowest = TRUE)) %>%
    group_by(ego_type, bin) %>%
    summarise(
      # midpoint of each bin for the x-axis
      x_mid = as.numeric(bin) - 0.5 / nbins,
      y_med = median(.data[[metric]], na.rm = TRUE),
      y_q25 = quantile(.data[[metric]], 0.25, na.rm = TRUE),
      y_q75 = quantile(.data[[metric]], 0.75, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = x_mid, y = y_med)) +
    geom_ribbon(aes(ymin = y_q25, ymax = y_q75), alpha = 0.15) +
    geom_line() +
    geom_point(size = 0.7) +
    facet_wrap(~ ego_type, nrow = 1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Binned median & IQR:", metric, "vs cross_sett_share"),
      x = "cross_sett_share", y = metric) +
    theme_minimal(base_size = 11)
}

# 2) Density-aware scatter: hexbin (no overplotting)
viz_hex <- function(df, metric, bins = 40) {
  ggplot(df, aes(x = cross_sett_share, y = .data[[metric]])) +
    geom_hex(bins = bins, show.legend = TRUE) +
    facet_wrap(~ ego_type, nrow = 1) +
    scale_x_continuous(labels = percent) +
    scale_fill_viridis_c(option = "C", trans = "log10") +
    labs(title = paste("Hexbin:", metric, "vs cross_sett_share"),
         x = "cross_sett_share", y = metric, fill = "count") +
    theme_minimal(base_size = 11)
}

# 3) Partial-residual view (control active days + total degree)
# Uses *_r rank columns; Pearson on residuals ~ partial Spearman
# Returns both a table and a plot
partial_residuals <- function(df, metric,
                              controls = c("sm_active_days_total_r", "sn_totaldegree_r")) {
  # Residualize rank (metric) and rank (cross_sett_share) on controls
  sub <- df %>%
    select(ego_type, cross_sett_share_r, all_of(paste0(metric, "_r")), all_of(controls)) %>%
    drop_na()
  if (nrow(sub) < 3) return(list(tab = tibble(), plot = ggplot() + theme_void()))

  rX <- resid(lm(cross_sett_share_r ~ ., data = sub[, c("cross_sett_share_r", controls)], drop = FALSE))
  rY <- resid(lm(sub[[paste0(metric, "_r")]] ~ ., data = sub[, c(paste0(metric, "_r"), controls)], drop = FALSE))

  plot_df <- tibble(ego_type = sub$ego_type, rX = as.numeric(rX), rY = as.numeric(rY))

  p <- ggplot(plot_df, aes(rX, rY)) +
    geom_hex(bins = 40) +
    facet_wrap(~ ego_type, nrow = 1) +
    scale_fill_viridis_c(option = "C", trans = "log10") +
    labs(title = paste("Partial residuals:", metric, "~ cross_sett_share | controls"),
         x = "residualized cross_sett_share (ranks)",
         y = paste("residualized", metric, "(ranks)")) +
    theme_minimal(base_size = 11)

  # Effect (overall + by ego type)
  eff <- bind_rows(
    plot_df %>%
      summarise(
        stratum  = "Overall",
        estimate = cor(rX, rY, method = "pearson"),
        n        = n()
      ),
    plot_df %>%
      group_by(ego_type) %>%
      summarise(
        stratum  = first(ego_type),
        estimate = cor(rX, rY, method = "pearson"),
        n        = n(), .groups = "drop"
      )
  )
  list(tab = eff, plot = p)
}

# 4) Extremes finder: label top x% by metric within each ego_type
viz_extremes <- function(df, metric, top_prop = 0.01) {
  cut_fun <- function(x, p) quantile(x, 1 - p, na.rm = TRUE)
  thr <- df %>% group_by(ego_type) %>%
    summarise(th = cut_fun(.data[[metric]], top_prop), .groups = "drop")
  df2 <- df %>% left_join(thr, by = "ego_type") %>%
    mutate(flag = .data[[metric]] >= th)
  ggplot(df2, aes(cross_sett_share, .data[[metric]])) +
    geom_hex(bins = 40) +
    geom_point(data = df2 %>% filter(flag), color = "red", size = 0.4, alpha = 0.5) +
    facet_wrap(~ ego_type, nrow = 1) + 
    scale_x_continuous(labels = percent) +
    scale_fill_viridis_c(option = "C", trans = "log10") +
    labs(title = paste("Extremes (top", percent(top_prop), "):", metric),
         x = "cross_sett_share",
         y = metric) +
    theme_minimal(base_size = 11)
}

# Examples

# A) Betweenness story
viz_binned(ego_df, "sn_betweenness")
viz_binned(ego_df, "sn_outdegree")
viz_binned(ego_df, "sn_indegree")
viz_binned(ego_df, "sn_totaldegree")
viz_binned(ego_df, "sm_entr_total")
#viz_hex(ego_df, "sn_betweenness")
#ext_plot <- viz_extremes(ego_df, "sn_betweenness", top_prop = 0.005) # top 0.5%

glimpse(ego_df)

ego_df_shares <- ego_df %>%
  rename(own = own_share,
         cross_sett = cross_sett_share,
         Alter: Big City = to_big_share)

ego_df_shares_long <- ego_df_shares %>%
  pivot_longer(
    cols = ends_with("_share"),
    names_to = "shares_to",
    values_to = "shares",
    # strip optional "to_" prefix and the trailing "_share"
    names_transform = list(
      shares_to = ~ sub("_share$", "", sub("^to_", "", .x))
    )
  )

glimpse(ego_df_shares_long)

# Factors
ego_df_shares_long$ego_type <- factor(ego_df_shares_long$ego_type,
                                      levels = c("B", "S", "R"),
                                      labels = c("Ego: Big City", "Ego: Small City", "Ego: Rural"))

ego_df_shares_long$shares_to <- factor(ego_df_shares_long$shares_to,
                                      levels = c("big", "small", "rural"),
                                      labels = c("Alter: Big City", "Alter: Small City", "Alter: Rural"))

glimpse(ego_df_shares_long)

# sm_rog_total

ggplot(ego_df_shares_long, aes(shares, sm_rog_total)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sm_nul_total

ggplot(ego_df_shares_long, aes(shares, sm_nul_total)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sm_entr_total

ggplot(ego_df_shares_long, aes(shares, sm_entr_total)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sm_active_days_total

ggplot(ego_df_shares_long, aes(shares, sm_active_days_total)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_totaldegree

ggplot(ego_df_shares_long, aes(shares, sn_totaldegree)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_indegree

ggplot(ego_df_shares_long, aes(shares, sn_indegree)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_outdegree

ggplot(ego_df_shares_long, aes(shares, sn_outdegree)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_betweenness

ggplot(ego_df_shares_long, aes(shares, sn_betweenness)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_closeness

ggplot(ego_df_shares_long, aes(shares, sn_closeness)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_clustering

ggplot(ego_df_shares_long, aes(shares, sn_clustering)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

# sn_reciprocity

ggplot(ego_df_shares_long, aes(shares, sn_reciprocity)) +
  geom_point(size = 0.4, alpha = 0.5) +
  facet_grid(shares_to ~ ego_type)

ggplot(ego_df, aes(cross_sett_share, to_big_share)) +
  #geom_ribbon(aes(ymin = y_q25, ymax = y_q75), alpha = 0.15) +
  #geom_line() +
  geom_point(size = 0.7) +
  facet_wrap(~ ego_type) +
  #scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "to_big_share",
    x = "cross_sett_share", y = "to_big_share") +
  theme_minimal(base_size = 11)

# Fast "dashboard" for all metrics

# Generate & save a small set for each metric:
for (m in metrics) {
  print(viz_binned(ego_df, m))
  #print(viz_hex(ego_df, m))
  pr <- partial_residuals(ego_df, m)
  print(pr$plot)
  print(pr$tab)
}

ggplot(ego_df, aes(ego, to_big_share)) + geom_point() + facet_wrap(~ ego_type)

ggplot(ego_df, aes(cross_sett_share, sn_outdegree), alpha = 0.5) + geom_point() + facet_wrap(~ ego_type)

ggplot(ego_df, aes(cross_sett_share, sn_betweenness), alpha = 0.5) + geom_point() + facet_wrap(~ ego_type)

ggplot(ego_df, aes(own_share, sm_rog_total), alpha = 0.5) + geom_point() + facet_wrap(~ ego_type)
