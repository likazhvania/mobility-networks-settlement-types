# =============================================================================
# Development script - not part of the final analysis
#
# Within-rural cell tower density gradient: a robustness check explored
# during the second review round and not reported.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

library(tidyverse)
library(sf)

# Anchor site per user -------------------------------------------------------

# use a backup version of 'edges_pass'
edges_pass_bu <- edges_pass

home_sites <- edges_pass_bu %>%
  left_join(analysis_data %>% select(caller = user_id, cg_domset),
            by = "caller") %>%
  filter(settlement_hierarchy == cg_domset) %>%
  count(caller, site_id, name = "n_events") %>%
  group_by(caller) %>%
  mutate(tied = sum(n_events == max(n_events)) > 1) %>%
  slice_max(n_events, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(caller, home_site_id = site_id, anchor_events = n_events, tied)

cat("share of users with a tied anchor:",
    round(mean(home_sites$tied), 3), "\n")
cat("users with an anchor:", nrow(home_sites),
    "of", nrow(analysis_data), "\n")

# Grain per site -------------------------------------------------------------

sites <- edges_pass_bu %>%
  group_by(site_id, settlement_type, settlement_hierarchy, x_coord, y_coord) %>%
  summarise(n = n()) %>% select(-n) %>%
  ungroup()

sites_m <- sites %>%
  select(site_id, x_coord, y_coord)

knn_dist_mean <- function(x, y, k = 3, chunk = 500) {
  n <- length(x)
  out <- numeric(n)
  for (start in seq(1, n, by = chunk)) {
    idx <- start:min(start + chunk - 1, n)
    d2 <- outer(x[idx], x, "-")^2 + outer(y[idx], y, "-")^2
    d2[cbind(seq_along(idx), idx)] <- Inf     # exclude each site from itself
    out[idx] <- apply(d2, 1, function(r)
      mean(sqrt(sort.int(r, partial = 1:k)[1:k])))
  }
  out
}

K_NN <- 3

sites_m$grain_m <- knn_dist_mean(sites_m$x_coord, sites_m$y_coord, k = K_NN)

cat("sites sharing coordinates:", sum(sites_m$grain_m == 0), "\n")

site_class <- edges_pass_bu %>% distinct(site_id, settlement_hierarchy)  # if sites lacks it

sites_m %>%
  left_join(site_class, by = "site_id") %>%
  group_by(settlement_hierarchy) %>%
  summarise(n_sites      = n(),
            grain_median = median(grain_m),
            grain_q1     = quantile(grain_m, .25),
            grain_q3     = quantile(grain_m, .75),
            .groups = "drop")

# use backup version of 'analysis_data'
analysis_data_bu <- analysis_data

rural <- analysis_data_bu %>%
  filter(cg_domset == "rural") %>%
  left_join(home_sites, by = c("user_id" = "caller")) %>%
  left_join(sites_m %>% select(site_id, grain_m),
            by = c("home_site_id" = "site_id")) %>%
  filter(!is.na(grain_m), !is.na(sm_rog_total))

nrow(rural)
quantile(rural$grain_m, c(0, .1, .25, .5, .75, .9, 1))

rural <- rural %>% mutate(grain_d = ntile(grain_m, 10))

gradient <- rural %>%
  group_by(grain_d) %>%
  summarise(n            = n(),
            grain_median = median(grain_m),
            rog_median   = median(sm_rog_total),
            rog_q1       = quantile(sm_rog_total, .25),
            rog_q3       = quantile(sm_rog_total, .75),
            entr_median  = median(sm_entr_total, na.rm = TRUE),
            nul_median   = median(sm_nul_total, na.rm = TRUE),
            days_median  = median(sm_active_days_total, na.rm = TRUE),
            .groups = "drop")

print(gradient, n = 10)

cor.test(rural$grain_m, rural$sm_rog_total, method = "spearman", exact = FALSE)

rog_weighted <- edges_pass_bu %>%
  mutate(xc = x_coord - 500000, yc = y_coord - 6500000) %>%  # any constants, for numerical safety
  group_by(caller) %>%
  summarise(sm_rog_total_w = sqrt(pmax(0, mean(xc^2) + mean(yc^2) - mean(xc)^2 - mean(yc)^2)),
            .groups = "drop")

rog_unweighted <- edges_pass_bu %>%
  distinct(caller, site_id, x_coord, y_coord) %>%
  mutate(xc = x_coord - 500000, yc = y_coord - 6500000) %>%
  group_by(caller) %>%
  summarise(rog_check = sqrt(pmax(0, mean(xc^2) + mean(yc^2) - mean(xc)^2 - mean(yc)^2)),
            .groups = "drop")

chk <- analysis_data_bu %>% select(user_id, sm_rog_total) %>%
  left_join(rog_unweighted, by = c("user_id"= "caller"))
max(abs(chk$sm_rog_total - chk$rog_check), na.rm = TRUE)

chk <- chk %>% mutate(d = abs(sm_rog_total - rog_check))

sum(is.na(chk$rog_check))      # users missing from the new computation
sum(chk$d > 1, na.rm = TRUE)   # how many differ at all
mean(chk$d > 1, na.rm = TRUE)  # as a share
quantile(chk$d, c(.5, .9, .99, 1), na.rm = TRUE)
sum(chk$rog_check > chk$sm_rog_total, na.rm = TRUE)  # is the difference directional?

sum(is.na(chk$sm_rog_total))
sum(is.na(chk$sm_rog_total) & is.na(chk$rog_check))
chk %>% filter(d > 1) %>% arrange(desc(d)) %>% head(5)

analysis_data_bu %>%
  left_join(rog_weighted, by = c("user_id" = "caller")) %>%
  group_by(cg_domset) %>%
  summarise(n      = n(),
            rog_unw_median = median(sm_rog_total, na.rm = TRUE),
            rog_w_median   = median(sm_rog_total_w, na.rm = TRUE),
            .groups = "drop")

sum(is.na(edges_pass_bu$x_coord) | is.na(edges_pass_bu$y_coord))

na_users <- edges_pass_bu %>% filter(is.na(x_coord) | is.na(y_coord)) %>% distinct(caller) %>% pull(caller)
length(na_users)
mean(chk$user_id[is.na(chk$rog_check)] %in% na_users)

edges_pass_bu %>% distinct(site_id, x_coord, y_coord) %>% count(site_id) %>% filter(n > 1) %>% nrow()

sum(analysis_data_bu$sm_nul_total == 1)

edges_pass_bu %>% distinct(site_id, x_coord, y_coord) %>%
  count(x_coord, y_coord) %>% filter(n > 1) %>% nrow()

chk <- analysis_data_bu %>% select(user_id, sm_rog_total) %>%
  left_join(rog_unweighted, by = c("user_id" = "caller")) %>%
  mutate(d = abs(sm_rog_total - rog_check))
sum(is.na(chk$rog_check)); max(chk$d, na.rm = TRUE)

## OLD -----------------------------------------------------------------------

# Distinct towers from the cell data. A tower = a unique site_id with its coords.
# (site_id is the physical mast; cell_id can be multiple sectors per site.)
towers <- mob_data_bu %>%
  distinct(site_id, longitude, latitude, settlement, municipality, county, settlement_type, settlement_hierarchy) %>%
  filter(!is.na(longitude), !is.na(latitude))

nrow(towers)   # total distinct towers in the data

# Assign each tower to a settlement type by locating its coordinates
# within the EHAL settlement polygons

# settlements: Land Board polygons with the TYYP column and geometry.
# To make sure both layers share the same CRS. Estonian national grid = EPSG:3301
# (L-EST97), which is projected in metres - needed for area in km2.

# check crs
class(settlements)
st_crs(settlements)

# make towers spatial
towers_sf <- st_as_sf(towers, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3301)

tower_counts <- towers %>%
  count(settlement_hierarchy) %>%
  mutate(n_towers = n) %>%
  select(-n)

tower_counts

area_by_class <- settlements %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(settlement_hierarchy) %>%
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop")

area_by_class

area_by_class <- area_by_class %>%
  mutate(
    settlement_hierarchy = case_when(
      settlement_hierarchy == "Big city" ~ "Urban: big city",
      settlement_hierarchy == "Small city" ~ "Urban: small city",
      settlement_hierarchy == "Rural" ~ "rural"

    )
  )

area_by_class

density_tbl <- tower_counts %>%
  left_join(area_by_class, by = "settlement_hierarchy") %>%
  mutate(
    towers_per_km2 = n_towers / area_km2,
    km2_per_tower = area_km2 / n_towers   # coarser resolution = larger value
  )

density_tbl

settlements_area <- settlements %>%
  mutate(settlement_id = row_number(),
         area_km2 = as.numeric(st_area(geometry)) / 1e6)

# Count towers per settlement polygon ----------------------------------------

# st_intersects returns, for each settlement, the indices of towers inside it; ----

# #length() turns that into a count. Zero-tower settlements get 0.

# Join
counts <- length(st_intersects(settlements_area, towers_sf))

# drop geometry, attach the counts, compute density
settlement_density <- settlements_area %>%
  st_drop_geometry() %>%
  mutate(
    n_towers  = counts,
    towers_per_km2 = n_towers / area_km2,
    km2_per_tower = if_else(n_towers > 0, area_km2 / n_towers, NA_real_)
  )

# label: zero-tower settlements
settlement_density_sum <- settlement_density %>%
  group_by(settlement_hierarchy) %>%
  summarise(
    n_settlements = n(),
    n_zero_tower = sum(n_towers == 0),
    pct_zero_tower = round(100 * mean(n_towers == 0), 1),
    .groups = "drop"
  )

# density distribution by class
settlement_density_stats <- settlement_density %>%
  group_by(settlement_hierarchy) %>%
  summarise(
    n_settlements = n(),
    median_towers_per_km2 = median(towers_per_km2, na.rm=TRUE),
    iqr_towers_per_km2 = IQR(towers_per_km2, na.rm=TRUE),
    median_km2_per_tower = median(km2_per_tower, na.rm=TRUE),
    iqr_km2_per_tower = IQR(km2_per_tower, na.rm=TRUE),
    .groups = "drop"
  )

settlement_density_stats
