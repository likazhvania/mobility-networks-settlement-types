# =============================================================================
# 18  Cell tower density by settlement type
#
# Describes how densely cell towers cover each settlement type; sparser
# towers mean coarser location data. Two views:
#   - per settlement type: number of distinct tower sites, total area, towers
#     per km2 and km2 per tower
#   - per settlement: towers inside each settlement polygon, summarised by
#     settlement type (settlements without a tower; median and IQR density)
#
# Input:  mob_data_bu (script 02); settlements (script 01)
# Output: density_tbl - density per settlement type; settlement_density_sum
#         and settlement_density_stats - per-settlement summaries
# =============================================================================

library(tidyverse)
library(sf)

# Distinct tower sites: one row per site_id with its coordinates and
# settlement. A site is one physical location; it can hold several cells
# (cell_id), for example one per antenna sector.
towers <- mob_data_bu %>%
  distinct(site_id, longitude, latitude, settlement, municipality, county, settlement_type, settlement_hierarchy) %>%
  filter(!is.na(longitude), !is.na(latitude))

nrow(towers)   # number of distinct tower sites

# settlements: Land Board settlement polygons from script 01. Both layers
# are used in the Estonian national grid (EPSG:3301, L-EST97), which is in
# metres, so that areas can be calculated in km2.

# Coordinate reference system of the settlement layer
class(settlements)
st_crs(settlements)

# Tower sites as points, transformed from WGS84 to EPSG:3301
towers_sf <- st_as_sf(towers, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3301)

# Tower sites per settlement type, using the settlement type already
# attached to the communication event data
tower_counts <- towers %>%
  count(settlement_hierarchy) %>%
  mutate(n_towers = n) %>%
  select(-n)

tower_counts

# Total area of each settlement type
area_by_class <- settlements %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(settlement_hierarchy) %>%
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop")

area_by_class

# Same labels as in the communication event data
area_by_class <- area_by_class %>%
  mutate(
    settlement_hierarchy = case_when(
      settlement_hierarchy == "Big city" ~ "Urban: big city",
      settlement_hierarchy == "Small city" ~ "Urban: small city",
      settlement_hierarchy == "Rural" ~ "rural"

    )
  )

area_by_class

# Towers per km2 and km2 per tower for each settlement type
density_tbl <- tower_counts %>%
  left_join(area_by_class, by = "settlement_hierarchy") %>%
  mutate(
    towers_per_km2 = n_towers / area_km2,
    km2_per_tower = area_km2 / n_towers   # coarser resolution = larger value
  )

density_tbl

## Towers per settlement -----------------------------------------------------

# Area of every settlement polygon
settlements_area <- settlements %>%
  mutate(settlement_id = row_number(),
         area_km2 = as.numeric(st_area(geometry)) / 1e6)

# st_intersects() lists, for each settlement, the tower sites inside it.
# length() returns the length of that list, which is the number of
# settlements (a single value), not the number of towers in each settlement.
counts <- length(st_intersects(settlements_area, towers_sf))

# Density per settlement
settlement_density <- settlements_area %>%
  st_drop_geometry() %>%
  mutate(
    n_towers  = counts,
    towers_per_km2 = n_towers / area_km2,
    km2_per_tower = if_else(n_towers > 0, area_km2 / n_towers, NA_real_)
  )

# Settlements without a tower, by settlement type
settlement_density_sum <- settlement_density %>%
  group_by(settlement_hierarchy) %>%
  summarise(
    n_settlements = n(),
    n_zero_tower = sum(n_towers == 0),
    pct_zero_tower = round(100 * mean(n_towers == 0), 1),
    .groups = "drop"
  )

# Median and IQR of tower density by settlement type
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
