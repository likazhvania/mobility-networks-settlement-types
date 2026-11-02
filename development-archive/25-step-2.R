# =============================================================================
# Development script - not part of the final analysis
#
# Earlier version of the connection-level analysis (script 15).
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)

# Start from raw events (edges_pass) -----------------------------------------

# make a date
edges <- edges_pass %>%
  mutate(date = as.Date(timestamp))

edges$callee <- as.character(edges$callee)

# settlement lookup (doinant type per user)
lkp <- analysis_data %>%
  select(user_id, cg_domset) %>%
  mutate(cg_domset = factor(cg_domset,
                            levels = c("Urban: big city", "Urban: small city", "rural")))

# A1 -------------------------------------------------------------------------

# Directed tie measures (one row per caller->callee) -------------------------

# weight + active days (directed)
dir_base <- edges %>%
  group_by(caller, callee) %>%
  summarise(weight = n(),
            days   = n_distinct(date), .groups = "drop")

# add ego/alter settlement and define a 6-class label (orderless text for readability)
dir_ties <- dir_base %>%
  left_join(lkp, by = c("caller" = "user_id")) %>% rename(dom_caller = cg_domset) %>%
  left_join(lkp, by = c("callee" = "user_id")) %>% rename(dom_callee = cg_domset) %>%
  filter(!is.na(dom_caller), !is.na(dom_callee)) %>%
  mutate(class6 = paste(pmin(as.character(dom_caller), as.character(dom_callee)),
                        pmax(as.character(dom_caller), as.character(dom_callee)),
                        sep = "-"))

# Reciprocity/direction balance

# find the reverse weight to compute reciprocity on the dyad
rev_w <- dir_base %>% select(caller = callee, callee = caller, w_rev = weight)
dir_ties <- dir_ties %>%
  left_join(rev_w, by = c("caller", "callee")) %>%
  mutate(w_rev = replace_na(w_rev, 0L),
         recip = if_else(weight > 0 & w_rev > 0, 1L, 0L),
         dir_balance = (weight - w_rev) / pmax(weight + w_rev, 1L))

# Now we can compare medians/IQR of weight, days, recip, and dir_balance by
# class6 wiht rank tests or bootstrap Cls. This answers "Do within-type vs
# cross-type ties differ?"

# Collapse to undirected dyads (one row per unordered pair) ------------------

# This avoids double-counting when we compare endpoints, while retaining
# reciprocity computed above.

und_ties <- dir_ties %>%
  transmute(u = pmin(caller, callee),
            v = pmax(caller, callee),
            class6,
            weight = weight + w_rev,     # total calls both ways
            days   = days,               # we can also take pmax(days, days_rev)
            recip  = recip) %>%
  group_by(u, v, class6) %>%
  summarise(weight = max(weight),
            days   = max(days),
            recip  = max(recip), .groups = "drop")

und_ties <- und_ties %>%
  mutate(class = class6)

und_ties$class <- factor(und_ties$class,
                         levels = c("Urban: big city-Urban: big city",
                                    "Urban: small city-Urban: small city",
                                    "rural-rural",
                                    "Urban: big city-Urban: small city",
                                    "rural-Urban: big city",
                                    "rural-Urban: small city"),
                         labels = c("BB", "SS", "RR", "BS", "BR", "SR"))

## To Visualise --------------------------------------------------------------

### Setup --------------------------------------------------------------------

library(purrr)

DF <- und_ties %>%
  mutate(class = class6,
         weight_log1p = log1p(weight))

# nice class order: within-types first, then cross-types
DF$class <- factor(DF$class,
                   levels = c("Urban: big city-Urban: big city",
                              "Urban: small city-Urban: small city",
                              "rural-rural",
                              "Urban: big city-Urban: small city",
                              "rural-Urban: big city",
                              "rural-Urban: small city"),
                   labels = c("BB", "SS", "RR", "BS", "BR", "SR"))

### Strength / persistence by class (violin + box) ---------------------------

p_weight <- ggplot(DF, aes(class, weight_log1p, fill = class)) + 
  geom_violin(trim = TRUE, alpha = .7, color = NA) +
  geom_boxplot(width = .15, outlier.alpha = .15, fill = "white", color = "grey20") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(x = NULL, y = "Total calls, log1p",
       title = "Tie strength by settlement pair (one row per dyad)") +
  theme_minimal(base_size = 11)

p_days <- ggplot(DF, aes(class, days, fill = class)) +
  geom_violin(trim = TRUE, alpha = .7, color = NA) +
  geom_boxplot(width = .15, outlier.alpha = .15, fill = "white", color = "grey20") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(x = NULL, y = "Active communication days (1-28)",
       title = "Tie persistence by settlement pair") +
  theme_minimal(base_size = 11)

p_weight
p_days

# Weight is heavy-tailed, hence log1p. Every tie has at least one event, so
# log1p only changes the scale.

### reciprocity rate by class (mean + Wilson CI) -----------------------------

# Wilson CI for a proportion
wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf)/2)
  p <- x / n
  denom <- 1 + z^2/n
  centre <- (p + z^2/(2*n)) / denom
  half <- (z * sqrt((p*(1-p) + z^2/(4*n))/n)) / denom
  c(lo = pmax(0, centre - half), hi = pmin(1, centre + half))
}

recip_sum <- DF %>%
  group_by(class) %>%
  summarise(n = n(),
            recip_n = sum(recip, na.rm=TRUE),
            recip_rate = recip_n / n,
            .groups = "drop") %>%
  rowwise() %>%
  mutate(ci_lo = wilson_ci(recip_n, n)[1],
         ci_hi = wilson_ci(recip_n, n)[2]) %>%
  ungroup()

p_recip <- ggplot(recip_sum, aes(class, recip_rate)) +
  geom_col(fill = "#4C78A8") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(x = NULL, y = "Reciprocal ties (%)",
       title = "Reciprocity by settlement pair (Wilson 95% CI)") +
  theme_minimal(base_size = 11)

p_recip

### Descriptive table for the paper (medians, IQR, rates) --------------------

# class_order <- c("BB", "SS", "RR", "BS", "BR", "SR")

summ_table <- DF %>%
  group_by(class) %>%
  summarise(
    n = n(),
    weight_median = median(weight, na.rm=TRUE),
    weight_IQR    = IQR(weight, na.rm=TRUE),
    days_median   = median(days, na.rm=TRUE),
    days_IQR      = IQR(days, na.rm=TRUE),
    recip_rate    = mean(recip, na.rm=TRUE)
  ) %>%
  mutate(recip_rate = scales::percent(recip_rate, accuracy = 0.1)) # %>% arrange(match(class, class_order))

summ_table

### Simple tests -------------------------------------------------------------

# Kruskal-Wallis + BH pairwise Wilcoxon (weight, days)

# Kruskal-Wallis
kw_weight <- kruskal.test(weight ~ class, data = DF)
kw_days   <- kruskal.test(days   ~ class, data = DF)

# pairwise Wilcoxon with Benjamini-Hochberg adjustment
pw_weight <- pairwise.wilcox.test(DF$weight, DF$class, p.adjust.method = "BH")
pw_days   <- pairwise.wilcox.test(DF$days,   DF$class, p.adjust.method = "BH")

kw_weight; kw_days
pw_weight$p.value
pw_days$p.value

### (Optional) Robustness to multiple ties per ego ---------------------------

# Because many dyads share egos, we can show that the pattern is stable by
# repeating th summaries on a subsample with dyad per ego equal or less 1.

set.seed(42)
# pick one random partner per ego
one_per_ego <- DF %>%
  group_by(u) %>%
  slice_sample(n = 1) %>%
  ungroup()

# re-run medians/plots/tests on 'one_per_ego' the same way as DF

# Even a single robustness run is enough to demonstrate the direction of
# differences is unchanged.

# EXAMPLES

# A2 Paired comparison inside each class -------------------------------------

# --- Inputs we already have ---
# und_ties: u, v, class6, class (orderless dyads, u < v)
# analysis_data: user_id, cg_domset, SN + SM metrics

# 0) Quick sanity checks (run once)
stopifnot(all(c("u", "v", "class6") %in% names(und_ties)))
stopifnot(all(c("user_id", "cg_domset") %in% names(analysis_data)))

# 1) Make endpoint metric tables (suffic _u/_v)
metrics <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", 
             "sn_betweenness", "sn_closeness", "sn_clustering", "sn_reciprocity",
             "sm_rog_total", "sm_nul_total", "sm_entr_total", 'sm_active_days_total')

nodes_u <- analysis_data |>
  select(user_id, cg_domset, all_of(metrics)) |>
  rename_with(~paste0(.x,"_u"), -user_id)

nodes_v <- analysis_data |>
  select(user_id, cg_domset, all_of(metrics)) |>
  rename_with(~paste0(.x,"_v"), -user_id)

# 2) Build dyad_long (keeps class + which end is typeA/typeB)

map <- c(B = "Urban: big city", S = "Urban: small city", R = "rural")

dyad_long <- und_ties |>
  left_join(nodes_u, by = c("u" = "user_id")) |>
  left_join(nodes_v, by = c("v" = "user_id")) |>
  mutate(
    class_chr = as.character(class),
    typeA = unname(map[substr(class_chr, 1, 1)]),
    typeB = unname(map[substr(class_chr, 2, 2)]),
    endA  = if_else(cg_domset_u == typeA, "u", "v"),
    endB  = if_else(endA == "u", "v", "u")
  )

# sanity checks
stopifnot(!any(is.na(dyad_long$typeA) | is.na(dyad_long$typeB)))
ok <- (dyad_long$cg_domset_u %in% c(dyad_long$typeA, dyad_long$typeB)) &
      (dyad_long$cg_domset_v %in% c(dyad_long$typeA, dyad_long$typeB))
stopifnot(all(ok))

# 3) Compute signed deltas per metric and keep class
make_delta <- function(df, m){
  a <- if_else(df$endA == "u",
              df[[paste0(m, "_u")]],
              df[[paste0(m, "_v")]])
  b <- if_else(df$endB == "u",
              df[[paste0(m, "_u")]],
              df[[paste0(m, "_v")]])
  tibble(class = df$class, metric = m, delta = a - b)
}

dyad_delta <- purrr::map_dfr(metrics, \(m) make_delta(dyad_long, m))

dyad_delta_sample <- dyad_delta %>%
  filter(metric == "sn_outdegree") %>%
  mutate(row = row_number())

ggplot(dyad_delta_sample, aes(delta, row, group = class)) +
  geom_line(size = 0.01) +
  #geom_point() +
  facet_wrap(~ class, scales = "free") +
  labs(x = '')

#ggplot(dyad_delta, aes(metric, delta)) +
  #geom_point() +
  #facet_wrap(~ class)

# What to compute (quick, robust summaries)

# - Per metric x class: median signed difference, IQR, and the share of ties
#   where the sign is positive (i.e., the endpoint of the first type in the
#   class has the larger value).

# - Similarity (absolute difference): use delta to assess how alike endpoints
#   are within a class.

# - Sign tests: within each class, test whether the median signed delta
#   differs from 0.

# - Across-class tests: compare deltas across the six classes (Kruskal-Wallis
#   + BH-adjusted pairwise Wilcoxon).

# The measures are on different scales, so a scaled delta is used to
# compare them. A simple, transparent choice is z_delta =
# delta / sd_metric, where sd_metric is the SD of that metric over all
# endpoints.

library(tidyverse)
library(purrr)

# 0) SD per metric for scaling
metric_sds <- dyad_delta %>%
  group_by(metric) %>%
  summarise(sd_metric = sd(delta, na.rm = TRUE), .groups = "drop")

dyad_delta2 <- dyad_delta %>%
  left_join(metric_sds, by = "metric") %>%
  mutate(z_delta   = delta / sd_metric,
         abs_delta = abs(delta),
         abs_z     = abs(z_delta),
         pos       = delta > 0)

# 1) Per metric x class summary
delta_summ <- dyad_delta2 %>%
  group_by(metric, class) %>%
  summarise(n = n(),
            med_delta = median(delta, na.rm=TRUE),
            iqr_delta = IQR(delta, na.rm=TRUE),
            p_pos     = mean(pos, na.rm=TRUE),
            med_abs   = median(abs_delta, na.rm=TRUE),
            iqr_abs   = IQR(abs_delta, na.rm=TRUE),
            med_abs_z = median(abs_z, na.rm=TRUE),
            .groups = "drop")

delta_summ

glimpse(delta_summ)

# med_delta
ggplot(data = delta_summ, aes(class, med_delta)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'med_delta'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# iqr_delta
ggplot(data = delta_summ, aes(class, iqr_delta)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'iqr_delta'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# p_pos
ggplot(data = delta_summ, aes(class, p_pos)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'p_pos'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# med_abs
ggplot(data = delta_summ, aes(class, med_abs)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'med_abs'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# iqr_abs
ggplot(data = delta_summ, aes(class, iqr_abs)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'iqr_abs'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# med_abs_z
ggplot(data = delta_summ, aes(class, med_abs_z)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'med_abs_z'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# 2) Within-class: is median delta not equal 0 ? (sign test via binomial on pos share)
sign_tests <- dyad_delta2 %>%
  group_by(metric, class) %>%
  summarise(n = sum(!is.na(pos)),
            k = sum(pos, na.rm=TRUE),
            p_binom = ifelse(n>0, binom.test(k, n, p = 0.5)$p.value, NA_real_),
            .groups = "drop") %>%
  group_by(metric) %>%
  mutate(p_adj = p.adjust(p_binom, method = "BH")) %>%
  ungroup()

sign_tests

glimpse(sign_tests)

ggplot(data = sign_tests, aes(class, n)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'n'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(data = sign_tests, aes(class, k)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'k'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(data = sign_tests, aes(class, p_binom)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'p_binom'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggplot(data = sign_tests, aes(class, p_adj)) +
  geom_point() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Parameter: 'p_adj'") +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# 3) Across-class: do classes differ in |delta|?
kw <- dyad_delta2 %>%
  group_by(metric) %>%
  summarise(H = kruskal.test(abs_delta ~ class)$statistic,
            p = kruskal.test(abs_delta ~ class)$p.value,
            .groups = "drop")

pair_grid <- combn(unique(dyad_delta2$class), 2, simplify = FALSE)
pair_tests <- map_dfr(pair_grid, function(cls){
  sub <- dyad_delta2 %>% filter(class %in% cls)
  tibble(metric = unique(sub$metric),
         g1 = cls[1], g2 = cls[2],
         p = map_dbl(split(sub, sub$metric),
                     ~ wilcox.test(abs_delta ~ class, data = .x)$p.value))
}) %>%
  group_by(metric) %>%
  mutate(p_BH = p.adjust(p, "BH")) %>%
  ungroup()

glimpse(kw)

kw

glimpse(pair_tests)

library(scales)

# 1) Median |delta| per class to add direction-of-differe ce (which class has larger (delta))
med_abs_by <- dyad_delta2 %>%
  group_by(metric, class) %>%
  summarise(med_abs = median(abs_delta, na.rm = TRUE), .groups = "drop")

# 2) Attach medians to each pairwise row and compute winner & gap
pair_viz <- pair_tests %>%
  left_join(med_abs_by, by = c("metric","g1"="class")) %>% rename(med1 = med_abs) %>%
  left_join(med_abs_by, by = c("metric","g2"="class")) %>% rename(med2 = med_abs) %>%
  mutate(diff = med1 - med2,
         winner = ifelse(med1 > med2, g1, g2),
         sig = p_BH < 0.05,
         strength = ifelse(sig, -log10(p_BH), 0))   # for colour scale

# 3) Heatmap: colour = significance strength; label = winner (class with larger |delta|)
ggplot(pair_viz, aes(g1, g2, fill = strength)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(sig, winner, "")), size = 3) +
  scale_fill_gradient(low = "grey95", high = "grey20",
                      breaks = c(0,1,3,6),
                      labels = c("ns", "1", "3", "6"),
                      name = expression("-log"[10]*"(adj p)")) +
  #coord_fixed() +
  facet_wrap(~ metric, scales = "free") +
  labs(x = NULL, y = NULL,
       title = "Pairwise differences in endpoint dissimilarity (|delta|) by dyad class",
       subtitle = "Tile shade = significance; label = class with larger median |delta|") +
  theme_minimal(base_size = 11)

# The label on the tile is the class that has larger median (delta) (i.e., endpoints are more dissiilar in that class).

# Visualisations that work in a journal

# A. Signed-delta per metric x class (who is larger?) Violin + median dot,
# zero line helps interpretation.

ggplot(dyad_delta2, aes(class, delta)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_violin(fill = "grey92", colour = "grey40", trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  labs(title = "Signed differences (endpoint A - endpoint B) by dydad class",
       y = "Delta (raw scale)", x = NULL) +
  theme_minimal(base_size = 11)

# Interpretation: if the median is above zero for, say, BR in sn_outdegree,
# the first type in "B-R" (big-city endpoint) tends to have higher out-degree
# than the rural endpoint.

# B. Similarity (absolute differences) by class

ggplot(dyad_delta2, aes(class, abs_delta)) +
  geom_violin(fill = "grey92", colour = "grey40", trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  labs(title = "Endpoint similarity within ties (delta by class)",
       y = "Delta (raw scale)", x = NULL) +
  theme_minimal(base_size = 11)

# Optional "one-number" view (across metrics): use abs_z and facet by nothing,
# or plot per class with error bars from a bootstrap.

sim_overall <- dyad_delta2 %>%
  group_by(class) %>%
  summarise(med_abs_z = median(abs_z, na.rm=TRUE),
            iqr_abs_z = IQR(abs_z, na.rm=TRUE), .groups = "drop")

ggplot(sim_overall, aes(reorder(class, med_abs_z), med_abs_z)) +
  geom_point() +
  geom_errorbar(aes(ymin = med_abs_z - 0.5*iqr_abs_z,
                    ymax = med_abs_z + 0.5*iqr_abs_z), width = .15) +
  coord_flip() +
  labs(title = "How similar are endpoints overall?",
       y = "Median delta (z-scaled across metrics)", x = NULL) +
  theme_minimal(base_size = 11)

# C. "Who is larger?" as percentages

pos_share <- dyad_delta2 %>%
  group_by(metric, class) %>%
  summarise(p_pos = mean(delta > 0, na.rm = TRUE), .groups = "drop")

ggplot(pos_share, aes(class, p_pos)) +
  geom_col(fill = "grey80", colour = "grey40") +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "grey50") +
  coord_flip() +
  facet_wrap(~ metric, ncol = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Share of ties where the first type in class has the larger value",
       y = "% with delta > 0", x = NULL) +
  theme_minimal(base_size = 11)
