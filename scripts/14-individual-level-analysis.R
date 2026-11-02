# =============================================================================
# 14  Individual-level analysis
#
# Compares users grouped by their dominant settlement type: big city (B),
# small city (S) and rural (R). Two parts:
#   1. Users' own settlement type: medians and IQRs of the mobility and
#      network measures in each group, Kruskal-Wallis tests and pairwise
#      Wilcoxon tests with Benjamini-Hochberg adjustment.
#   2. Settlement composition of users' contacts: the event-weighted share
#      of each user's contacts in each settlement type; tests across groups
#      defined by the contacts' dominant settlement type; linear models and
#      GAMs of the measures on the contact shares.
#
# Results are reported in Tables 2-4, Figure 5 and Tables S1-S11.
#
# Input:  analysis_data (script 08); directed_edges_weighted and
#         undirected_edges_weighted (script 04)
# Output: result tables printed to the console; per_ego and df2 - contact
#         shares and measures per user
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(purrr)

# Checks on the edge lists: TRUE when every undirected pair is sorted
# (u <= v) and appears only once

all(undirected_edges_weighted$u <= undirected_edges_weighted$v) &&
  !any(duplicated(undirected_edges_weighted[c("u", "v")]))

# The two totals are equal when the directed weights add up to the
# undirected ones

sum(directed_edges_weighted$weight)
sum(undirected_edges_weighted$weight)

# 1. Users' own settlement type ----------------------------------------------

## Data preparation ----------------------------------------------------------

# One row per user: settlement group and all measures
nodes <- analysis_data %>%
  select(user_id, cg_domset,
         sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity,
         sm_rog_total, sm_nul_total, sm_entr_total, sm_active_days_total) %>%
  mutate(cg_domset = factor(cg_domset, levels = c("Urban: big city", "Urban: small city", "rural")))

# Short group codes: B, S, R

map_dom <- c("Urban: big city"="B", "Urban: small city"="S", "rural"="R")
nodes <- nodes %>% mutate(type = recode(cg_domset, !!!map_dom))

# Heavy-tailed measures, meant for a log scale in the boxplots below
heavy <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness",
           "sm_rog_total", "sm_nul_total")

nodes_long <- nodes %>%
  pivot_longer(-c(user_id, cg_domset, type), names_to = "metric", values_to = "value") #%>%
#mutate(
#value_t = ifelse(metric %in% heavy, log1p(value), value),
#scale_note = ifelse(metric %in% heavy, "(log1p)", ""),
#metric_lab = paste0(metric, " ", scale_note)
#)

# Boxplots of every measure by group (exploratory). metric_lab is created
# only in the commented-out lines above, so the plot runs only with those
# lines active.
bxpl <- ggplot(nodes_long, aes(type, value, fill = type)) + 
  geom_boxplot() +
  scale_fill_manual(breaks = c("B", "S", "R"), 
                    values = c("#b77466ff", "#ffe1afff", "#957c62ff")) +
  facet_wrap(~ metric_lab, scales = "free") +
  labs(xlab("value"),
       ylab("DomSet"))

bxpl

## Users per group -----------------------------------------------------------

# Number and percentage of users in each group

ego_domset <- nodes %>%
  group_by(cg_domset, type) %>%
  summarise(n = n(),
            perc = round(n*100/nrow(nodes))) %>%
  ungroup()

ego_domset

# Reported in Table S1 and in the note to Table 2.

## Measures by group: medians and IQRs ---------------------------------------

# Median and interquartile range of every measure in each group

summ <- nodes_long %>%
  group_by(metric, cg_domset, type) %>%
  summarise(median = median(value, na.rm=TRUE),
            median_rnd = round(median, 2),
            IQR = IQR(value, na.rm=TRUE),
            IQR_rnd = round(IQR, 2),
            .groups = "drop")

summ

knitr::kable(summ)

# Reported in Table 2 and Table S2.

# Medians and IQRs by group, plotted for a visual check

ggplot(summ, aes(type, median_rnd)) +
  geom_point() +
  facet_wrap(~metric, scales = "free")

ggplot(summ, aes(type, IQR_rnd)) +
  geom_point() +
  facet_wrap(~metric, scales = "free")

glimpse(nodes)

## Differences between groups ------------------------------------------------

# Kruskal-Wallis test of each measure across the three groups

library(dplyr)
library(purrr)
library(tidyr)

metrics <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness", "sn_closeness", "sn_clustering", "sn_reciprocity",
             "sm_rog_total", "sm_nul_total", "sm_entr_total", "sm_active_days_total")

kw_results <- map_dfr(metrics, function(m) {
  f <- as.formula(paste(m, "~ cg_domset"))
  kt <- kruskal.test(f, data = nodes)

  tibble(
    metric    = m,
    statistic = unname(kt$statistic),
    df        = unname(kt$parameter),
    p_value   = kt$p.value,
    p_value_rnd = round(p_value, 4)
  )
})

kw_results

# Reported in Table S3.

# Pairwise Wilcoxon tests between the groups, Benjamini-Hochberg adjusted

posthoc_results <- map(metrics, function(m) {
  x <- nodes[[m]]
  ph <- pairwise.wilcox.test(x, nodes$cg_domset,
                             p.adjust.method = "BH")
  list(metric = m, p_values = ph$p.value)
})

posthoc_results

# The same results as one table: metric | group1 | group2 | p_value

posthoc_df <- map_dfr(posthoc_results, function(res) {
  as.data.frame(as.table(res$p_values)) %>%      # Var1, Var2, Freq
    filter(!is.na(Freq)) %>%
    transmute(
      metric = res$metric,
      group1 = Var1,
      group2 = Var2,
      p_value = Freq,
      p_value_rnd = round(p_value, 4)
    )
})

posthoc_df

# Reported in Table S4.

knitr::kable(posthoc_df, digits = 4)

# 2. Settlement composition of contacts --------------------------------------

## Contact shares per user ---------------------------------------------------

# For each user (ego), the shares of their contacts (alters) in big cities,
# small cities and rural areas, weighted by the number of communication events
# on each tie.

# Packages

library(tidyverse)
library(purrr)
library(broom)
library(scales)

# Settlement group of every user

nodes <- analysis_data %>%
  select(user_id, cg_domset) %>% distinct()

# Short group codes: B, S, R

map_dom <- c("Urban: big city"="B", "Urban: small city"="S", "rural"="R")
nodes <- nodes %>% mutate(type = recode(cg_domset, !!!map_dom))

# Undirected ties
edges_std <- undirected_edges_weighted

edges_std$u <- as.character(edges_std$u)
edges_std$v <- as.character(edges_std$v)

# Each pair appears once; both directions are added so that every user has
# one row per contact
edges_long <- bind_rows(
  edges_std %>% transmute(ego = u, alter = v, across(-c(u,v), identity)),
  edges_std %>% transmute(ego = v, alter = u, across(-c(u,v), identity))
)

# Settlement group of both users; pairs with a missing group are dropped

ego_alter <- edges_long %>%
  left_join(nodes %>% select(user_id, type) %>% rename(ego_type = type),
            by = c("ego" = "user_id")) %>%
  left_join(nodes %>% select(user_id, type) %>% rename(alter_type = type),
            by = c("alter" = "user_id")) %>%
  filter(!is.na(ego_type), !is.na(alter_type))

# Ties are weighted by their number of communication events

wt <- "weight"
#if (!wt %in% names(ego_alter)) ego_alter[[wt]] <- 1

# Shares per user: own group, other groups, and each of B, S and R

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

per_ego$ego_type <- factor(per_ego$ego_type, levels = c("B", "S", "R"))

# The example ego networks in Figure 5b belong to three users, one from each
# group, selected from per_ego.

glimpse(per_ego)

# Median and IQR of the contact shares in each group

ego_alter_summ <- per_ego %>%
  group_by(ego_type) %>%
  summarise(
    #med_n_alters         = median(n_alters),
    #med_w_sum            = median(w_sum),
    #med_own_w            = median(own_w),
    #med_toB_w            = median(toB_w),
    #med_toS_w            = median(toS_w),
    #med_toR_w            = median(toR_w),
    med_own_share        = median(own_share),
    med_cross_sett_share = median(cross_sett_share),
    med_to_big_share     = median(to_big_share),
    med_to_small_share   = median(to_small_share),
    med_to_rural_share   = median(to_rural_share),

    #IQR_n_alters         = IQR(n_alters),
    #IQR_w_sum            = IQR(w_sum),
    #IQR_own_w            = IQR(own_w),
    #IQR_toB_w            = IQR(toB_w),
    #IQR_toS_w            = IQR(toS_w),
    #IQR_toR_w            = IQR(toR_w),
    IQR_own_share        = IQR(own_share),
    IQR_cross_sett_share = IQR(cross_sett_share),
    IQR_to_big_share     = IQR(to_big_share),
    IQR_to_small_share   = IQR(to_small_share),
    IQR_to_rural_share   = IQR(to_rural_share),
  ) %>%
  ungroup()

# Long format

ego_alter_summ_long <- ego_alter_summ %>%
  pivot_longer(-c(ego_type), names_to = "measure", values_to = "value")

ego_alter_summ

ego_alter_summ_long

glimpse(ego_alter_summ)

# Reported in Figure 5a and Table S5.

## Measures by the contacts' dominant settlement type ------------------------

# Contact shares joined with each user's mobility and network measures

ego_df <- per_ego %>%
  left_join(
    analysis_data %>%
      select(user_id,
             sm_rog_total, sm_nul_total, sm_entr_total, sm_active_days_total,
             sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity),
    by = c("ego" = "user_id")
  )

# Dominant settlement type of each user's contacts: the type with the largest
# share. max.col() breaks ties between equal shares at random.

df2 <- ego_df %>%
  mutate(alt_dom = c("B", "S", "R")[max.col(cbind(to_big_share, to_small_share, to_rural_share))])

# Within each user group, the measures are compared across the contacts'
# dominant settlement types

metrics <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness", "sn_reciprocity",
             "sm_rog_total", "sm_nul_total", "sm_entr_total")

# Big-city users
sub_B <- df2 %>% filter(ego_type == "B")

# Small-city users
sub_S <- df2 %>% filter(ego_type == "S")

# Rural users
sub_R <- df2 %>% filter(ego_type == "R")

# Kruskal-Wallis tests within each user group
kw_results_B <- map_dfr(metrics, function(m) {
  f <- as.formula(paste(m, "~ alt_dom"))
  kt <- kruskal.test(f, data = sub_B)

  tibble(
    metric    = m,
    statistic = unname(kt$statistic),
    df        = unname(kt$parameter),
    p_value   = kt$p.value,
    p_value_rnd = round(p_value, 4)
  )
})

kw_results_S <- map_dfr(metrics, function(m) {
  f <- as.formula(paste(m, "~ alt_dom"))
  kt <- kruskal.test(f, data = sub_S)

  tibble(
    metric    = m,
    statistic = unname(kt$statistic),
    df        = unname(kt$parameter),
    p_value   = kt$p.value,
    p_value_rnd = round(p_value, 6)
  )
})

kw_results_R <- map_dfr(metrics, function(m) {
  f <- as.formula(paste(m, "~ alt_dom"))
  kt <- kruskal.test(f, data = sub_R)

  tibble(
    metric    = m,
    statistic = unname(kt$statistic),
    df        = unname(kt$parameter),
    p_value   = kt$p.value,
    p_value_rnd = round(p_value, 4)
  )
})

kw_results_B
kw_results_S
kw_results_R

kw_results_B

kw_results_S

kw_results_R

# Pairwise Wilcoxon tests, Benjamini-Hochberg adjusted
posthoc_results_B <- map(metrics, function(m) {
  x <- sub_B[[m]]
  ph <- pairwise.wilcox.test(x, sub_B$alt_dom,
                             p.adjust.method = "BH")

  list(metric = m, p_values = ph$p.value)
})

posthoc_results_S <- map(metrics, function(m) {
  x <- sub_S[[m]]
  ph <- pairwise.wilcox.test(x, sub_S$alt_dom,
                             p.adjust.method = "BH")
  list(metric = m, p_values = ph$p.value)
})

posthoc_results_R <- map(metrics, function(m) {
  x <- sub_R[[m]]
  ph <- pairwise.wilcox.test(x, sub_R$alt_dom,
                             p.adjust.method = "BH")
  list(metric = m, p_values = ph$p.value)
})

posthoc_results_B

posthoc_results_S

posthoc_results_R

# The same results as tables
posthoc_df_B_alters <- map_dfr(posthoc_results_B, function(res) {
  as.data.frame(as.table(res$p_values)) %>%      # Var1, Var2, Freq
    filter(!is.na(Freq)) %>%
    transmute(
      metric = res$metric,
      group1 = Var1,
      group2 = Var2,
      p_value = Freq,
      p_value_rnd = round(p_value, 4)
    )
})

posthoc_df_S_alters <- map_dfr(posthoc_results_S, function(res) {
  as.data.frame(as.table(res$p_values)) %>%      # Var1, Var2, Freq
    filter(!is.na(Freq)) %>%
    transmute(
      metric = res$metric,
      group1 = Var1,
      group2 = Var2,
      p_value = Freq,
      p_value_rnd = round(p_value, 4)
    )
})

posthoc_df_R_alters <- map_dfr(posthoc_results_R, function(res) {
  as.data.frame(as.table(res$p_values)) %>%      # Var1, Var2, Freq
    filter(!is.na(Freq)) %>%
    transmute(
      metric = res$metric,
      group1 = Var1,
      group2 = Var2,
      p_value = Freq,
      p_value_rnd = round(p_value, 4)
    )
})

# posthoc_df_B_alters

knitr::kable(posthoc_df_B_alters, digits = 4)
knitr::kable(posthoc_df_S_alters, digits = 4)
knitr::kable(posthoc_df_R_alters, digits = 4)

# Reported in Table 3 and Table S6 (Kruskal-Wallis) and Table S7 (pairwise
# tests).

## Regression on the contact shares ------------------------------------------

# The contact shares enter as continuous predictors, with an interaction by
# user group. The three shares sum to one, so the rural share is left out.
# Big city (B) is the reference group.

# Example: linear model for spatial entropy

fit_entr <- lm(
  sm_entr_total ~ ego_type * (to_big_share + to_small_share),
  data = df2
)

summary(fit_entr)

library(mgcv)

# Example: GAM for spatial entropy, with a smooth term for each share in each
# user group

fit_entr_gam <- gam(
  sm_entr_total ~ ego_type + s(to_big_share, by = ego_type) + s(to_small_share, by = ego_type),
  data = df2
)

summary(fit_entr_gam)

# All measures: one model per measure

library(dplyr)
library(purrr)
library(broom)
library(mgcv)

# Measures modelled

metrics <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness", "sn_reciprocity", "sm_rog_total", "sm_nul_total", "sm_entr_total")

# Linear models: coefficients

lm_results <- map_dfr(metrics, function(m) {
  form <- as.formula(paste(m, "~ ego_type * (to_big_share + to_small_share)"))
  fit <- lm(form, data = df2)

  tidy(fit) %>%    # coefficients table
    mutate(metric = m, .before = 1)
})

lm_results

knitr::kable(lm_results)

# Reported in Table 4 and Table S8.

# Linear models: fit statistics (R2, AIC, F-test)

lm_glance <- map_dfr(metrics, function(m) {
  form <- as.formula(paste(m, "~ ego_type * (to_big_share + to_small_share)"))
  fit <- lm(form, data = df2)

  glance(fit) %>% mutate(metric = m, .before = 1)
})

lm_glance

# Reported in Table S9.

# GAMs for all measures

gam_fits <- map(metrics, function(m) {
  form <- as.formula(
    paste(m, "~ ego_type + s(to_big_share, by = ego_type) + s(to_small_share, by = ego_type)")
  )
  gam(form, data = df2)
})

names(gam_fits) <- metrics

# Summaries and partial-effect plots per measure

summary(gam_fits[["sm_nul_total"]])
plot(gam_fits[["sm_entr_total"]])   # partial effects of shares, by type

gam_fits

summary(gam_fits[["sn_totaldegree"]])

summary(gam_fits[["sn_indegree"]])

summary(gam_fits[["sn_outdegree"]])

summary(gam_fits[["sn_betweenness"]])

summary(gam_fits[["sn_reciprocity"]])

summary(gam_fits[["sm_rog_total"]])

summary(gam_fits[["sm_nul_total"]])

summary(gam_fits[["sm_entr_total"]])

# Reported in Table S10.

# Parametric terms of all GAMs as one table

gam_param <- map_dfr(metrics, function(m) {
  sm <- summary(gam_fits[[m]])
  as.data.frame(sm$p.table) %>%
    mutate(term  = rownames(sm$p.table),
           metric = m,
           .before = 1)
})

gam_param

# Reported in Table S11.
