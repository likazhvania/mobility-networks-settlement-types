# =============================================================================
# 01  Spatial data preparation
#
# Loads the administrative and population layers for Estonia and classifies
# every settlement unit into one of the three settlement types used in the
# study, based on its official EHAK code:
#   - big city:   the six principal urban centres (Tallinn, Tartu, Narva,
#                 Pärnu, Kohtla-Järve and Jõhvi)
#   - small city: all other urban settlements
#   - rural:      villages
# All layers are projected to the Estonian national grid, L-EST97 (EPSG:3301).
#
# Input:  Estonian Land Board shapefiles in data/spatial/ - counties,
#         municipalities and settlement units (2022), population grid (2019)
# Output: counties, municipalities, settlements and population2019, kept in
#         the R session for the scripts that follow
# =============================================================================

# Setup ----------------------------------------------------------------------

# sf handles spatial vector data; tidyverse covers data manipulation (dplyr)
# and plotting (ggplot2).
library(sf)
library(tidyverse)

# Load the spatial layers ----------------------------------------------------

# Every layer is transformed to the same coordinate reference system as soon
# as it is read, so that all later spatial operations are consistent.

# Target CRS: L-EST97 (EPSG:3301), a projected system in metres suited to
# spatial analysis within Estonia.
target_crs <- 3301

# Layers used:
# - Counties: administrative boundaries of the counties (2022)
# - Municipalities: administrative boundaries of the municipalities (2022)
# - Settlement units: boundaries of cities, towns, city districts, small
#   towns and villages (2022), carrying the EHAK settlement type codes
# - Population (2019): population counts on a 1 km grid, used for context only

# --- Administrative boundaries (2022) ---
# Source: Estonian Land Board (Maa-amet)

# Counties
counties <- st_read('data/spatial/maakond_20220601.shp') %>% 
  st_transform(crs = target_crs)

# Municipalities
municipalities <- st_read('data/spatial/omavalitsus_20220601.shp') %>% 
  st_transform(crs = target_crs)

# Settlement units (2022)
settlements <- st_read('data/spatial/asustusyksus_20220601.shp') %>% 
  st_transform(crs = target_crs)

# --- Population grid (2019) ---
# Used only for contextual maps.
population2019 <- st_read('data/spatial/asustus_rahvaarv_2019.shp') %>% 
  st_transform(crs = target_crs)

# Classify settlement types --------------------------------------------------

# Two variables are derived from the EHAK codes (Estonian Classification of
# Administrative Units and Settlements):
#   settlement_type       urban or rural
#   settlement_hierarchy  big city, small city or rural - the classification
#                         used throughout the study
#
# Relevant columns in the settlement layer:
#   TYYP   EHAK settlement type code
#   ONIMI  name of the municipality the settlement unit belongs to
# Source of the classification: Estonian Land Board Geoportal
# (geoportaal.maaamet.ee/eng/Spatial-Data/Administrative-and-Settlement-Division-p312.html)

# EHAK settlement type codes:
# 0 - county
# 1 - rural municipality
# 3 - town
# 4 - city
# 5 - city without municipal status
# 6 - city district
# 7 - small town
# 8 - village

# Settlement type codes present in the data.
cat('The codes indicating the types of settlements of Estonia, are following:\n', sort(as.integer(unique(settlements$TYYP))))

# --- Create 'settlement_type' (Urban vs. Rural) ---
settlements <- settlements %>%
  mutate(
    settlement_type = case_when(
      TYYP %in% c(3, 4, 5, 6, 7)    ~ "urban",         # Codes for cities and towns
      TYYP == 8                     ~ "rural",         # Code for villages
      TRUE                          ~ NA_character_    # Default case
    )
  )

# --- Create 'settlement_hierarchy' (Big City, Small City, Rural) ---
settlements <- settlements %>% 
  mutate(
    # Cities, cities without municipal status and city districts (codes 4-6)
    # that belong to the six principal urban centres become big cities. City
    # districts of Tallinn and Kohtla-Järve are thereby counted with their
    # parent city.
    settlement_hierarchy = case_when(
      ONIMI %in% c("Tallinn", "Narva linn", "Kohtla-Järve linn", "Tartu linn", 
                   "Pärnu linn", "Jõhvi vald") & 
        TYYP %in% c(4, 5, 6) ~ "Urban: big city", 

      settlement_type == "urban" ~ "Urban: small city", 
      # All other urban settlements become small cities

      TRUE ~ settlement_type # Villages stay rural
    )
  )

# --- Check the resulting classes ---
cat("Unique values in 'settlement_type':\n")
print(unique(settlements$settlement_type))

cat("Unique values in 'settlement_hierarchy':\n")
print(unique(settlements$settlement_hierarchy))

# Check maps -----------------------------------------------------------------

# Maps of the main layers, to confirm that the data loaded correctly and that
# the classification is geographically sensible.

# --- Plot Settlement Hierarchy ---
ggplot() + 
  # Plot county boundaries as a grey background layer
  geom_sf(data = counties, fill = "grey90", color = "white", size = 0.5) +
  # Settlements coloured by the new hierarchy variable
  geom_sf(data = settlements, aes(fill = settlement_hierarchy), color = NA) +
  # Manually define colours for clarity
  scale_fill_manual(
    name = "Settlement Hierarchy",
    values = c("Urban: big city" = "#d73027", 
               "Urban: small city" = "#fc8d59", 
               "rural" = "#91bfdb")
  ) + 
  labs(
    title = "Settlement Hierarchy in Estonia",
    subtitle = "Classified based on EHAK codes and major urban areas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color="#525252", size=14), 
    plot.subtitle = element_text(color="grey50", size=10), 
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.grid = element_blank(), 
    panel.background = element_blank(),
    legend.position = "bottom"
  )

# --- Plot of Administrative Layers ---
# Nested administrative boundaries, to confirm that the layers align.
ggplot() +

  # Layer 1: Settlements (the finest grain)
  geom_sf(data = settlements, fill = "#EABE94", color = 'white', size = 0.001, alpha = 0.7) +
  # Layer 2: Municipalities (nested within counties)
  geom_sf(data = municipalities, fill = NA, color = "#969696", lwd = 0.5) +
  # Layer 3: Counties (the largest unit, as a base layer)
  geom_sf(data = counties, fill = NA, color = "#969696", size = 1) +

  geom_sf_text(data = counties, aes(label=str_sub(MNIMI, end = -8)), color='#737373', size=4) +
  labs(
    title = "Administrative Geography of Estonia",
    subtitle = "Counties, municipalities, and settlement unit locations (2022)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color="#525252", size=14), 
    plot.subtitle = element_text(color="grey50", size=10), 
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.grid = element_blank(), 
    panel.background = element_blank()
    #legend.position = "bottom"
  )

# --- Plot of Population Distribution ---
# Population across Estonia, with each grid cell filled by its population
# count.
ggplot(data = population2019) +
  geom_sf(
    aes(fill = VAARTUS), # 'VAARTUS': population count
    color = NA
  ) +
  # scale_fill_gradientn(colors=c('#ece2f0', '#a6bddb', '#3690c0', '#016c59', '#014636'), na.value = "#d9d9d9") +
  scale_fill_viridis_c(
    name = "Population\n(per sq.km)",
    na.value = "#d9d9d9",
    option = "mako",
    direction = -1,  # Reverse the scale
    trans = "log1p"  # Use a log scale for better visualisation of skewed data
  ) +
  labs(
    title = "Population Distribution in Estonia (2019)",
    subtitle = "Population count in 1km x 1km grid cells" 
    #fill = "Population number"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color="#525252", size=14), 
    plot.subtitle = element_text(color="grey50", size=10), 
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.grid = element_blank(), 
    panel.background = element_blank(),
    legend.position = "right"
  )
