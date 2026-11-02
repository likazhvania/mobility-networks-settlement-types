# =============================================================================
# 15  Connection-level analysis
#
# Compares ties by the settlement types of the two users they connect. Ties
# fall into six classes: within-type (BB, SS, RR) and cross-type (BS, BR,
# SR), where B = big city, S = small city and R = rural. Two parts:
#   1. Tie strength: number of communication events, active days and
#      reciprocity by class; Kruskal-Wallis tests and pairwise Wilcoxon tests
#      with Benjamini-Hochberg adjustment.
#   2. Similarity of connected users: the difference in every mobility and
#      network measure between the two ends of each tie. Signed differences
#      (sign tests) show which end tends to be higher; absolute differences
#      (Kruskal-Wallis and pairwise Wilcoxon tests) show how similar the two
#      ends are.
#
# Results are reported in Table 5 and Tables S12-S18.
#
# Input:  edges_pass (script 03); analysis_data (script 08)
# Output: und_ties - one row per undirected tie; dyad_delta2 - differences
#         per tie and measure; result tables printed to the console
# =============================================================================

# Setup ----------------------------------------------------------------------

library(tidyverse)
library(purrr)

# Ties from the event-level data ---------------------------------------------

# Date of each communication event
edges <- edges_pass %>%
  mutate(date = as.Date(timestamp))

edges$callee <- as.character(edges$callee)

# Dominant settlement type of every user
lkp <- analysis_data %>%
  select(user_id, cg_domset) %>%
  mutate(cg_domset = factor(cg_domset,
                            levels = c("Urban: big city", "Urban: small city", "rural")))

# 1. Tie strength by settlement pair -----------------------------------------

## Directed ties: one row per sender -> receiver -----------------------------

# Number of communication events (weight) and active days of each directed tie
dir_base <- edges %>%
  group_by(caller, callee) %>%
  summarise(weight = n(),
            days   = n_distinct(date), .groups = "drop")

# Settlement type of both ends, and the class of the pair. The two labels
# are sorted, so that A -> B and B -> A fall into the same class.
dir_ties <- dir_base %>%
  left_join(lkp, by = c("caller" = "user_id")) %>% rename(dom_caller = cg_domset) %>%
  left_join(lkp, by = c("callee" = "user_id")) %>% rename(dom_callee = cg_domset) %>%
  filter(!is.na(dom_caller), !is.na(dom_callee)) %>%
  mutate(class6 = paste(pmin(as.character(dom_caller), as.character(dom_callee)),
                        pmax(as.character(dom_caller), as.character(dom_callee)),
                        sep = "-"))

# Reciprocity and direction balance, from the weight of the reverse tie
# (receiver -> sender). A tie is reciprocal when communication events go both
# ways; dir_balance ranges from -1 to 1, with 0 for equal weight in both
# directions.
rev_w <- dir_base %>% select(caller = callee, callee = caller, w_rev = weight)
dir_ties <- dir_ties %>%
  left_join(rev_w, by = c("caller", "callee")) %>%
  mutate(w_rev = replace_na(w_rev, 0L),
         recip = if_else(weight > 0 & w_rev > 0, 1L, 0L),
         dir_balance = (weight - w_rev) / pmax(weight + w_rev, 1L))

## Undirected ties: one row per pair -----------------------------------------

# Each pair is counted once. weight is the total number of communication
# events in both directions, days the larger of the two directions' active
# days, and recip is carried over from above.

und_ties <- dir_ties %>%
  transmute(u = pmin(caller, callee),
            v = pmax(caller, callee),
            class6,
            weight = weight + w_rev,     # communication events in both directions
            days   = days,               # per direction; the larger value is kept below
            recip  = recip) %>%
  group_by(u, v, class6) %>%
  summarise(weight = max(weight),
            days   = max(days),
            recip  = max(recip), .groups = "drop")

# Short class codes: BB, SS, RR (within type); BS, BR, SR (cross type)
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

## Tie strength by class -----------------------------------------------------

library(purrr)

# Working copy with log-transformed weight
DF <- und_ties %>%
  mutate(class = class6,
         weight_log1p = log1p(weight))

# Class order: within-type first, then cross-type
DF$class <- factor(DF$class,
                   levels = c("Urban: big city-Urban: big city",
                              "Urban: small city-Urban: small city",
                              "rural-rural",
                              "Urban: big city-Urban: small city",
                              "rural-Urban: big city",
                              "rural-Urban: small city"),
                   labels = c("BB", "SS", "RR", "BS", "BR", "SR"))

### Events and active days by class (violin and box plots) -------------------

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

# Weight is heavy-tailed, hence log1p. Every tie has at least one
# communication event, so log1p only changes the scale.

### Reciprocity by class (share, Wilson 95% CI) ------------------------------

# Wilson confidence interval for a proportion
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

### Summary table: medians, IQRs and reciprocity rates -----------------------

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

# Reported in Table S12.

### Differences between classes ----------------------------------------------

# Kruskal-Wallis tests and pairwise Wilcoxon tests (Benjamini-Hochberg
# adjusted) for weight and active days

# Kruskal-Wallis
kw_weight <- kruskal.test(weight ~ class, data = DF)
kw_days   <- kruskal.test(days   ~ class, data = DF)

# Pairwise Wilcoxon tests, Benjamini-Hochberg adjusted
pw_weight <- pairwise.wilcox.test(DF$weight, DF$class, p.adjust.method = "BH")
pw_days   <- pairwise.wilcox.test(DF$days,   DF$class, p.adjust.method = "BH")

kw_weight
kw_days
pw_weight$p.value
pw_days$p.value

kw_weight
kw_days

pw_weight$p.value
pw_days$p.value

# Reported in Table S13.

# 2. Similarity of connected users -------------------------------------------

## Differences between the two ends of each tie ------------------------------

# Uses und_ties (u, v, class6, class; u < v) and analysis_data (user_id,
# cg_domset and all measures).

# 0) Required columns
stopifnot(all(c("u", "v", "class6") %in% names(und_ties)))
stopifnot(all(c("user_id", "cg_domset") %in% names(analysis_data)))

# 1) Measures of both ends of each tie (suffixes _u and _v)
metrics <- c("sn_totaldegree", "sn_indegree", "sn_outdegree", 
             "sn_betweenness", "sn_closeness", "sn_clustering", "sn_reciprocity",
             "sm_rog_total", "sm_nul_total", "sm_entr_total", 'sm_active_days_total')

nodes_u <- analysis_data |>
  select(user_id, cg_domset, all_of(metrics)) |>
  rename_with(~paste0(.x,"_u"), -user_id)

nodes_v <- analysis_data |>
  select(user_id, cg_domset, all_of(metrics)) |>
  rename_with(~paste0(.x,"_v"), -user_id)

# 2) Ends A and B of each tie. A has the first settlement type of the class
#    code and B the second: in BR ties, for example, A is the big-city end
#    and B the rural end. In within-type ties (BB, SS, RR) A is simply u, the
#    user with the smaller ID.

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

# Checks: both types are found, and both ends belong to the class
stopifnot(!any(is.na(dyad_long$typeA) | is.na(dyad_long$typeB)))
ok <- (dyad_long$cg_domset_u %in% c(dyad_long$typeA, dyad_long$typeB)) &
      (dyad_long$cg_domset_v %in% c(dyad_long$typeA, dyad_long$typeB))
stopifnot(all(ok))

# 3) Signed difference per tie and measure: delta = A - B
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

# 4) Scaled and absolute differences. delta is divided by its standard
#    deviation per measure (z_delta), so that measures can be compared.
metric_sds <- dyad_delta %>%
  group_by(metric) %>%
  summarise(sd_metric = sd(delta, na.rm = TRUE), .groups = "drop")

dyad_delta2 <- dyad_delta %>%
  left_join(metric_sds, by = "metric") %>%
  mutate(z_delta   = delta / sd_metric,
         abs_delta = abs(delta),
         abs_z     = abs(z_delta),
         pos       = delta > 0)

## Summary of the differences ------------------------------------------------

# Per measure and class: median and IQR of the signed and absolute
# differences, and the share of ties in which A is higher (p_pos)
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

knitr::kable(delta_summ, digits = 4)

# Reported in Table S14.

glimpse(delta_summ)

# Each summary statistic by class, one panel per measure

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

## Direction: sign tests -----------------------------------------------------

# Binomial test of whether A is higher than B in more (or fewer) than half of
# the ties, per measure and class. p-values are Benjamini-Hochberg adjusted
# within each measure.
sign_tests <- dyad_delta2 %>%
  group_by(metric, class) %>%
  summarise(n = sum(!is.na(pos)),
            k = sum(pos, na.rm=TRUE),
            p_binom = ifelse(n>0, binom.test(k, n, p = 0.5)$p.value, NA_real_),
            .groups = "drop") %>%
  group_by(metric) %>%
  mutate(p_adj = p.adjust(p_binom, method = "BH")) %>%
  ungroup()

# Reported in Table S15.

knitr::kable(sign_tests, digits = 4)

glimpse(sign_tests)

# Test results by class, one panel per measure
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

## Similarity: absolute differences across classes ---------------------------

# Kruskal-Wallis test of the absolute differences across the six classes,
# per measure
kw <- dyad_delta2 %>%
  group_by(metric) %>%
  summarise(H = kruskal.test(abs_delta ~ class)$statistic,
            p = kruskal.test(abs_delta ~ class)$p.value,
            .groups = "drop")

# Pairwise Wilcoxon tests between classes, Benjamini-Hochberg adjusted within
# each measure
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

# Reported in Table S16 (kw) and Table S17 (pair_tests).

kw

knitr::kable(kw, digits = 4)

glimpse(kw)

library(scales)

# Median absolute difference per measure and class
med_abs_by <- dyad_delta2 %>%
  group_by(metric, class) %>%
  summarise(med_abs = median(abs_delta, na.rm = TRUE), .groups = "drop")

# Reported in Table 5 and Table S18.

# Medians attached to each pairwise test; winner is the class with the larger
# median absolute difference
pair_viz <- pair_tests %>%
  left_join(med_abs_by, by = c("metric","g1"="class")) %>% rename(med1 = med_abs) %>%
  left_join(med_abs_by, by = c("metric","g2"="class")) %>% rename(med2 = med_abs) %>%
  mutate(diff = med1 - med2,
         winner = ifelse(med1 > med2, g1, g2),
         sig = p_BH < 0.05,
         strength = ifelse(sig, -log10(p_BH), 0))   # shading in the heatmap

# Heatmap of the pairwise tests: shade = significance; label = the class with
# the larger median absolute difference, i.e. the less similar endpoints
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

## Distribution plots --------------------------------------------------------

# A. Signed differences by class: violins with the median as a dot; the zero
#    line marks equal values

ggplot(dyad_delta2, aes(class, delta)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_violin(fill = "grey92", colour = "grey40", trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  labs(title = "Signed differences (endpoint A - endpoint B) by dydad class",
       y = "Delta (raw scale)", x = NULL) +
  theme_minimal(base_size = 11)

# A median above zero for BR in sn_outdegree, for example, means that the
# big-city end tends to have a higher out-degree than the rural end.

# B. Similarity (absolute differences) by class

ggplot(dyad_delta2, aes(class, abs_delta)) +
  geom_violin(fill = "grey92", colour = "grey40", trim = TRUE) +
  stat_summary(fun = median, geom = "point", size = 2) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  labs(title = "Endpoint similarity within ties (delta by class)",
       y = "Delta (raw scale)", x = NULL) +
  theme_minimal(base_size = 11)

# Overall similarity per class across all measures: median of the scaled
# absolute differences (abs_z), with bars of half the IQR either side

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

# C. Share of ties in which end A has the larger value

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
