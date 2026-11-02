# =============================================================================
# Development script - not part of the final analysis
#
# Settlement mixing matrix of ties (weighted by events) and Newman
# assortativity.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(purrr)

# Quick sanity check

# Should be TRUE if pairs are unique, sorted, and aggregated

all(undirected_edges_weighted$u <= undirected_edges_weighted$v) &&
  !any(duplicated(undirected_edges_weighted[c("u", "v")]))

# Verify that directed totals collapse to the undirected weight

sum(directed_edges_weighted$weight)
sum(undirected_edges_weighted$weight)

# lookup

lookup <- analysis_data %>%
  select(user_id, cg_domset) %>%
  mutate(cg_domset = factor(cg_domset, levels = c("Urban: big city", "Urban: small city", "rural")))

# Tag edges with the dominant settlement at both ends ------------------------

E <- undirected_edges_weighted %>%
  mutate(u = as.character(u),
         v = as.character(v)) %>%
  left_join(lookup, by = c("u" = "user_id")) %>% rename(dom_u = cg_domset) %>%
  left_join(lookup, by = c("v" = "user_id")) %>% rename(dom_v = cg_domset) %>%
  filter(!is.na(dom_u), !is.na(dom_v))

# 3x3 mixing matrix (weighted by calls) + Newman assortativity ---------------

# Weighted counts by category pair (undirected)
mix_tbl <- E %>%
  count(dom_u, dom_v, wt = weight, name = "w") %>%
  tidyr::complete(dom_u = levels(lookup$cg_domset),
                  dom_v = levels(lookup$cg_domset),
                  fill = list(w = 0))

# Put it in a 3x3 matrix in the usual order
cats <- levels(lookup$cg_domset)
M <- matrix(0, nrow = 3, ncol = 3, dimnames = list(cats, cats))
for (i in seq_along(cats)) for(j in seq_along(cats)) {
  # fill both triangles and diagonal
  w <- mix_tbl$w[mix_tbl$dom_u == cats[min(i,j)] & mix_tbl$dom_v == cats[max(i,j)]]
  M[i,j] <- if (i==j) w else (M[i,j] + w)
  M[j,i] <- M[i,j]
}

# Normalise to probabilities
P <- M / sum(M)

# Newman assortativity for normal attributes:
a <- rowSums(P); b <- colSums(P)    # identical here; undirected
r_newman <- (sum(diag(P)) - sum(a*b)) / (1 - sum(a*b))
r_newman

# Plot as a heatmap

as.data.frame(as.table(P)) %>%
  ggplot(aes(Var1, Var2, fill = Freq)) +
  geom_tile() + geom_text(aes(label = scales::percent(Freq, accuracy = 0.1))) +
  scale_fill_viridis_c(name = "share") +
  labs(x = "Ego dom. settlement", y = "Alter dom. settlement") +
  coord_equal() +
  theme_minimal(base_size = 11)

# Per-ego alter composition (weihted), summarised by ego type ----------------

# This gives the distribution of an ego's alters across domset, which can be
# reported as medians/IQR by ego type.

# Make a long "ego-alter" table so each edge contributes to both endpoints
EA <- bind_rows(
  E %>% transmute(ego = u, alter = v, ego_dom = dom_u, alter_dom = dom_v, weight),
  E %>% transmute(ego = v, alter = u, ego_dom = dom_v, alter_dom = dom_u, weight)
) %>% left_join(lookup, by = c("ego" = "user_id")) %>%
  rename(ego_dom_check = cg_domset) %>%
  mutate(ego_dom = coalesce(ego_dom, ego_dom_check)) %>%
  select(-ego_dom_check)

# Weighted alter shares per ego
ego_comp <- EA %>%
  group_by(ego, ego_dom, alter_dom) %>%
  summarise(w = sum(weight), .groups = "drop") %>%
  group_by(ego, ego_dom) %>%
  mutate(share = w / sum(w)) %>%
  ungroup()

# Summary for paper: typical alter mix by ego type
ego_comp_summary <- ego_comp %>%
  group_by(ego_dom, alter_dom) %>%
  summarise(median_share = median(share),
            iqr = IQR(share),
            .groups = "drop")
ego_comp_summary

# Sankey/alluvial input (for a single, clean diagram)

# Edge weights between ego type and alter type (weighted by calls)
sankey_df <- EA %>%
  group_by(ego_dom, alter_dom) %>%
  summarise(flow = sum(weight), .groups = "drop")

# Plot with ggalluvial::geom_alluvium() or a chord diagram (circlize)

# 1) Ensure consistent ordering and palette
lvl <- c("Urban: big city", "Urban: small city", "rural")
pal <- c("Urban: big city"="#122C43", "Urban: small city"="#C38F16", "rural"="#2C6B67")

df <- sankey_df %>%
  mutate(
    ego_dom   = factor(ego_dom,   levels = lvl),
    alter_dom = factor(alter_dom, levels = lvl)
  )

# 2) Block sizes (proportional heights) on each sides
left <- df %>% group_by(ego_dom) %>% summarise(size = sum(flow), .groups = "drop") %>%
  mutate(prop = size/sum(size)) %>%
  arrange(ego_dom) %>% mutate(ymin = lag(cumsum(prop), default = 0), ymax = cumsum(prop), side="ego")

right <-df %>% group_by(alter_dom) %>% summarise(size = sum(flow), .groups = "drop") %>%
  mutate(prop = size/sum(size)) %>%
  arrange(alter_dom) %>% mutate(ymin = lag(cumsum(prop), default = 0), ymax = cumsum(prop), side="alter") %>%
  rename(ego_dom = alter_dom)

# 3) Midpoints for link anchors
left_mid <- left   %>% transmute(ego_dom, y = (ymin+ymax)/2)
right_mid <- right %>% transmute(ego_dom, y = (ymin+ymax)/2) %>%
  rename(alter_dom = ego_dom, y_alter = y)

links <- df %>%
  left_join(left_mid, by = "ego_dom") %>%
  left_join(right_mid, "alter_dom")

# 4) Plot
x_left  <- 0.35
x_right <- 0.65

ggplot() +
  # left blocks
  geom_rect(data = left,
            aes(xmin = x_left-0.06, xmax = x_left-0.005, ymin = ymin, ymax = ymax, fill = ego_dom),
            color = "grey40") +
  # right blocks
  geom_rect(data = right,
            aes(xmin = x_right+0.005, xmax = x_right+0.06, ymin = ymin, ymax = ymax, fill = ego_dom),
            color = "grey40") +
  # links
  geom_segment(data = links,
               aes(x = x_left-0.005, xend = x_right+0.005, y = y, yend = y_alter, size = flow, color = ego_dom),
               alpha = .75, lineend = "round") +
  scale_size_continuous(range = c(0.5, 8), guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_color_manual(values = pal, guide = "none") +
  scale_y_continuous(expand = c(0.01, 0.01), labels = NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0.2, 0.8)) +
  annotate("text", x = x_left-0.085, y = 1.02, label = "Ego dominant settlement",   hjust = 0, size = 3.5) +
  annotate("text", x = x_left+0.085, y = 1.02, label = "Alter dominant settlement", hjust = 1, size = 3.5) +
  labs(title = "Call-weighted flows from ego to alter dominant settlement type",
       subtitle = "Block height totl calls by side; link thickness calls between types") +
  theme_minimal(base_size = 11) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank())

# DONT RUN -------------------------------------------------------------------

lvl <- c("Urban: big city", "Urban: small city", "rural")

ties <- EA

ties_plot <- ties %>%
  mutate(
    ego_dom   = factor(ego_dom,   levels = lvl),
    alter_dom = factor(alter_dom, levels = lvl),

    # x positions (left/right columns)
    x0 = 0,
    x1 = 1,

    # y positions: band by type (1..3) + small jitter so lines don't stack
y0 = as.numeric(ego_dom)   + runif(n(), -0.35, 0.35),
y1 = as.numeric(alter_dom) + runif(n(), -0.35, 0.35),

# simple visual scaling for heavy-tailed weights (optional)
w = if ("weight" %in% names(.)) log1p(weight) else 1
)

# (Optional) downsample for speed if have millions of dyads
# ties_plot <- ties_plot %>% slice_sample(n = 150000)

# Bands + labels for readability
bands <- tibble(
  grp = factor(lvl, levels = lvl),
  y = 1:3
)

ggplot() +
  # background bands left/right
  geom_rect(data = bands,
            aes(xmin = -0.08, xmax = -0.02, ymin = y-0.5, ymax = y+0.5),
            fill = "grey92", color = NA) +
  geom_rect(data = bands,
            aes(xmin = 1.02, xmax = 1.08, ymin = y-0.5, ymax = y+0.5),
            fill = "grey92", color = NA) +
  # labels
  geom_text(data = bands, aes(x = -0.09, y = y, label = grp),
            hjust = 1, size = 3.4) +
  geom_text(data = bands, aes(x = 1.09, y = y, label = grp),
            hjust = 0, size = 3.4) +
  # one curve per dyad
  geom_curve(
    data = ties_plot,
    aes(x = x0, y = y0, xend = x1, yend = y1,
        size = w, alpha = w),
    curvature = 0.25, lineend = "round", color = "steelblue3", show.legend = FALSE
  ) +
  scale_size_continuous(range = c(0.1, 1.8)) +
  scale_alpha_continuous(range = c(0.05, 0.7)) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.5, 3.5), expand = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 20, 10, 20)
  ) +
  labs(title = "Dyad-level flows between ego to alter dominant settlement types",
       subtitle = "On ecurve per dyad (edge); line width/alpha - log(1+calls)")

# Distributions --------------------------------------------------------------

# nodes: one row per user
nodes <- analysis_data %>%
  select(user_id, cg_domset,
         sn_totaldegree, sn_indegree, sn_outdegree, sn_betweenness, sn_closeness, sn_clustering, sn_reciprocity,
         sm_rog_total, sm_nul_total, sm_entr_total, sm_active_days_total) %>%
  mutate(cg_domset = factor(cg_domset, levels = c("Urban: big city", "Urban: small city", "rural")))

# One faceted plot for all metrics

# Pick which metric should be plotted on a log scale (heavy-tailed):
heavy <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", "sn_betweenness",
           "sm_rog_total", "sm_nul_total")

nodes_long <- nodes %>%
  pivot_longer(-c(user_id, cg_domset), names_to = "metric", values_to = "value") %>%
  mutate(
    value_t = ifelse(metric %in% heavy, log1p(value), value),
    scale_note = ifelse(metric %in% heavy, "(log1p)", ""),
    metric_lab = paste0(metric, " ", scale_note)
  )

ggplot(nodes_long, aes(cg_domset, value_t)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width=.14, outlier.alpha = .08) +
  facet_wrap(~ metric_lab, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = "Value (log1p where noted)",
       title = "Distributions of SN and SM metrics by dominant settlement type") +
  theme_minimal(base_size = 11)

summ <- nodes_long %>%
  group_by(metric, cg_domset) %>%
  summarise(median = median(value, na.rm=TRUE),
            IQR = IQR(value, na.rm=TRUE), .groups = "drop")

summ

## Non-parametric test -------------------------------------------------------

# Kruskal-Wallis across the three groups

metrics <- setdiff(names(nodes), c("user_id", "cg_domset"))

kw_res <- map_dfr(metrics, function(m){
  d <- nodes %>% filter(!is.na(.data[[m]]))
  kt <- kruskal.test(d[[m]] ~ d$cg_domset)
  tibble(metric = m,
         H = unname(kt$statistic),
         df = unname(kt$parameter),
         p = kt$p.value)
})

kw_res

# BH-adjusted pairwise Wilcoxon (post-hoc)

pw_res <- map_dfr(metrics, function(m){
  d <- nodes %>% filter(!is.na(.data[[m]]), !is.na(cg_domset))

  # need at least 2 groups to test
  if (dplyr::n_distinct(d$cg_domset) < 2) {
    return(tibble(metric = m, group1 = NA_character_, group2 = NA_character_, p_adj = NA_real_))
  }

  pw <- pairwise.wilcox.test(d[[m]], d$cg_domset, p.adjust.method = "BH")

  # extract upper triangle of the p-value matrix
  mat <- pw$p.value
  ut <- upper.tri(mat, diag = FALSE)

  tibble(
    metric = m,
    group1 = rownames(mat)[row(mat)[ut]],
    group2 = colnames(mat)[col(mat)[ut]],
    p_adj = as.vector(mat[ut])
  )
})

pw_res
