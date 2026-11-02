# =============================================================================
# Development script - not part of the final analysis
#
# Map of settlement types and the distribution of users by municipality.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# Setup ----------------------------------------------------------------------

# --- Setup ---
library(sf)
library(tidyverse)
library(dplyr)

# Source helpers (recommended)
# source("R/vis_helpers.R")

# Data (already loaded in the sessions)
# settlements, municipalities, counties, sample_* (will add sample later)

# --- CRS + bbox ---
target_crs <- 3301
settlements    <- st_transform(settlements, target_crs)
municipalities <- st_transform(municipalities, target_crs)
counties       <- st_transform(counties, target_crs)
population2019 <- st_transform(population2019, target_crs)

bb   <- st_bbox(counties)
xlim <- c(bb["xmin"], bb["xmax"])
ylim <- c(bb["ymin"], bb["ymax"])

# Settlement types -----------------------------------------------------------

# --- Settlement hierarchy factors ---
settlements <- settlements |>
  mutate(settlement_hierarchy = factor(
    settlement_hierarchy,
    levels = c("Urban: big city", "Urban: small city", "rural"),
    labels = c("Big city", "Small city", "Rural")
  ))

# --- Label a few major cities by parent name (ONIMI) ---
major <- c("Tallinn", "Tartu linn", "Narva linn", "Pärnu linn", "Kohtla-Järve linn", "JÕhvi linn")

city_labels <- settlements |>
  filter(ONIMI %in% major, settlement_hierarchy == "Big city") |>
  group_by(ONIMI) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  mutate(name_lab = sub(" linn$", "", ONIMI)) |>   # drop "linn" in labels
  ungroup()

# --- Plot: Settlement map (Panel A) ---

pal3 <- c("Big city"   = "#122C43",  # "#0072B2"
          "Small city" = "#C38F16",  # "#E69F00"
          "Rural"      = "#2C6B67")  # "#009E73"

p_settlements <- ggplot() +

  # Settlement polygons filled by 3-class hierarchy
  geom_sf(data = settlements, aes(fill = settlement_hierarchy), colour = NA) +

  # Municipality boundaries (thin, mid-grey
  geom_sf(data = municipalities, fill = NA, colour = "#252525", linewidth = 0.04, linetype=3) +

  # County boundaries (slightly thicker, darker)
  geom_sf(data = counties,       fill = NA, colour = "#525252", linewidth = 0.08) +

  # Major city labels (minimal overlap handling; keep to a small curated set)
  geom_sf_text(data = city_labels, aes(label = name_lab),
               size = 2, size.unit = "pt", colour = "#252525", vjust = -2.2,
               check_overlap = TRUE) +

  scale_fill_manual(values = pal3, name = "Settlement type", drop = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +

  labs(tag = "A") +

  theme_minimal(base_family = "sans") +

  theme(

    plot.tag           = element_text(face = "bold", size = 6, colour = "#252525"),
    #plot.tag.position  = c(),

    axis.title = element_blank(),
    axis.text  = element_blank(),

    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),

    legend.position = c(-0.11, -0.15),
    legend.direction = "horizontal",
    legend.justification = c("left", "bottom"),
    legend.box.just = "left",
    legend.box.background = element_blank(),

    #legend.box.margin = margin(t = -4, r = 0, b = 0, l =0),
    #legend.margin = margin(t = 0, r = 0, b = -3, l =0),
    #legend.spacing.y = unit(0, "mm"),

    legend.title.position = "top",
    legend.title       = element_text(size = 3, colour = "#252525", face = "bold"),
    legend.text        = element_text(size = 2, colour = "#525252"),
    legend.key.height  = unit(1.0, "mm"),
    legend.key.width   = unit(1.5, "mm"),
    legend.key.spacing = unit(1.0, "mm"),

    plot.background = element_rect(fill = "#F9F9EB", colour = NA),

    plot.margin        = margin(4, 4, 8, 5)  # mm around the figure
  )

# Title and subtitle, moved to the figure caption:
# title = "Settlement types in Estonia",
# subtitle = "EHAK settlements coloured by category: 4,713 units; \nadmin overlays: 79 municipalities, 15 counties"

p_settlements

# TIFF @ 1000 dpi
ggsave("figs/p_settlements.tiff", p_settlements, device = "tiff",
       width = 90, height = 60, units = "mm",
       dpi = 1000, compression = "lzw")

# Sample distribution calculations -------------------------------------------

# --- Population per municipality ---

# (point-on-surface is guaranteed inside the polygon; works for MULTIPOLYGON too)
population2019 <- st_make_valid(population2019)
municipalities <- st_make_valid(municipalities)

settlement_pts <- population2019 %>%
  mutate(pop = suppressWarnings(as.numeric(VAARTUS))) %>%
  st_point_on_surface() %>%
  select(settl_code = KOOD, pop, geometry)

# Join points to municipalities (within); fallback to nearest if any NA
settl_to_muni <- st_join(
  settlement_pts,
  municipalities %>% select(muni_id = OKOOD, muni_name = ONIMI),
  join = st_within
)

miss <- is.na(settl_to_muni$muni_id)
if (any(miss)) {
  settl_fix <- settlement_pts[miss, ] %>%
    st_join(municipalities %>% select(muni_id = OKOOD, muni_name = ONIMI),
            join = st_nearest_feature)
  settl_to_muni[miss, c("muni_id", "muni_name")] <- settl_fix[, c("muni_id", "muni_name")]
}

municipalities_pop <- settl_to_muni %>%
  st_drop_geometry() %>%
  group_by(muni_id, muni_name) %>%
  summarise(population = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  right_join(
    municipalities %>% select(muni_id = OKOOD, muni_name = ONIMI, geometry),
    by = c("muni_id", "muni_name")
  ) %>%
  mutate(population = ifelse(is.na(population), 0L, population)) %>%
  ungroup()

# --- Dominant municipality per caller (based on where they place most calls)
# Group by caller and municipality
callers_muni <- edges_pass %>%
  group_by(caller, municipality) %>%
  summarise(n_calls = n()) %>%
  ungroup()

# Choose ONE dominant municipality per caller:
callers_muni_dom <- callers_muni %>%
  arrange(caller, desc(n_calls), municipality) %>%
  group_by(caller) %>%
  slice(1) %>%
  ungroup() %>%
  select(caller, municipality)

# Group by municipalities, so each municipality has the number of samples
# (users who made the most calls from the particular municipality)
samples_muni <- callers_muni_dom %>%
  group_by(municipality) %>%
  summarise(n_samples = n()) %>%
  ungroup()

samples_muni <- samples_muni %>%
  mutate(muni_name = municipality)

glimpse(municipalities_pop)
glimpse(samples_muni)

# Join 'municipalities_pop' with 'samples_muni'
municipalities_pop <- municipalities_pop %>%
  left_join(samples_muni, by = "muni_name") %>%

  # Calculate the number of samples per 1,000 residents
  mutate(users_per_1000 = 1000 * n_samples / population)

# --- Tie check ---
callers_muni <- edges_pass %>%
  semi_join(analysis_data %>% distinct(user_id), by = c("caller" = "user_id")) %>%
  count(caller, municipality, name = "n_calls")

ties <- callers_muni %>%
  group_by(caller) %>%
  summarise(max_n = max(n_calls), t = sum(n_calls == max_n), .groups = "drop") %>%
  filter(t > 1) %>%
  ungroup()

n_ties <- nrow(ties)
share_t <- n_ties / n_distinct(callers_muni$caller)
cat("ties:", n_ties, " (", scales::percent(share_t), " of users)\n", sep = "")

ramp_blues   <- grDevices::colorRampPalette(c("#D2C4B9", "#CDBEB3", "#C6B3A9", "#A1827A", "#67322E"))

scale_fill_seq_blues <- function(n = 5, ...) scale_fill_gradientn(colours = ramp_blues(n), ...)

seq5 <- c("#E9F3E6", "#74C8C3", "#5A97C1", "#295384", "#0A2E57")
seq5 <- c("#EDE9DC", "#DCD1C5", "#C6B3A9", "#A1827A", "#67322E")
seq5 <- c("#D2C4B9", "#CDBEB3", "#C6B3A9", "#A1827A", "#67322E")

# 4) Sample map (Panel B)

# --- 1) Class breaks (5- or 6-class quantiles) ---
vals <- municipalities_pop$users_per_1000
vals <- vals[is.finite(vals)]
probs <- seq(0, 1, length.out = 6)    # 5 classes
brks <- unique(as.numeric(quantile(vals, probs, na.rm = TRUE)))
if (length(brks) < 6) brks <- pretty(vals, n = 5)   # fallback if duplicates

municipalities_pop <- municipalities_pop %>%
  mutate(users_per_1000_q = cut(users_per_1000,
                                breaks = brks,
                                include.lowest = TRUE))

# 'municipalities_pop' has the geometry dropped, so need to make it sf
municipalities_pop_sf <- municipalities %>%
  select(muni_id = OKOOD, muni_name = ONIMI, geometry) %>%
  left_join(municipalities_pop, by = c("muni_id", "muni_name"))

# --- 2) Panel B: Sample distribution by municipality ---
p_sample <- ggplot() +

  geom_sf(data = municipalities_pop_sf,
          aes(fill = users_per_1000_q),
          colour = "#d9d9d9", linewidth = 0.05) +   # , linetype=3

  # County boundaries
  #geom_sf(data = counties, fill = NA, colour = "#d9d9d9", linewidth = 0.05) +

  scale_fill_manual(
    values = ramp_blues(length(levels(municipalities_pop_sf$users_per_1000_q))),
    name = "Users per 1,000",
    na.translate = FALSE
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +

  labs(tag = "B") +

  theme_minimal(base_family = "sans") +

  theme(

    plot.tag           = element_text(face =  "bold", size = 6, colour = "#252525"),
    #plot.tag.position  = c(),

    axis.title = element_blank(),
    axis.text  = element_blank(),

    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),

    legend.position = c(-0.11, -0.15),
    legend.direction = "horizontal",
    legend.justification = c("left", "bottom"),
    legend.box.just = "left",
    legend.box.background = element_blank(),

    #legend.box.margin = margin(t = -4, r = 0, b = 0, l =0),
    #legend.margin = margin(t = 0, r = 0, b = -3, l =0),
    #legend.spacing.y = unit(0, "mm"),

    legend.title.position = "top",
    legend.title       = element_text(size = 3, colour = "#252525", face = "bold"),
    legend.text        = element_text(size = 2, colour = "#525252"),
    legend.key.height  = unit(1.0, "mm"),
    legend.key.width   = unit(1.5, "mm"),
    legend.key.spacing = unit(1.0, "mm"),

    plot.background = element_rect(fill = "#F9F9EB", colour = NA),

    plot.margin        = margin(4, 4, 8, 5)  # mm around the figure
  )

# Title, subtitle and caption, moved to the figure caption:
# title = "Sample distribution by municipality",
# subtitle = "Users per 1,000 residents (dominant municipality per user)",
# caption = "Ties (~2% of users) broken deterministically"

p_sample

# TIFF @ 1000 dpi
ggsave("figs/p_sample.tiff", p_sample, device = "tiff",
       width = 90, height = 60, units = "mm",
       dpi = 1000, compression = "lzw")

library(gridExtra)

p_settl_sample <- grid.arrange(p_settlements, p_sample, nrow = 1)

# TIFF @ 1000 dpi
ggsave("figs/p_settl_sample.tiff", p_settl_sample, device = "tiff",
       width = 90, height = 60, units = "mm",
       dpi = 1000, compression = "lzw")

# 5) Exports - separate masters
dir.create("figs", showWarnings = FALSE)

# Single-column master (90 mm)
# ggsave_pdf_ceus(p_settlements, "figs/fig01A_settlements_single.pdf",
#                 column = "single", height_mm = 110)

# Double-column master (190 mm)
# ggsave_pdf_ceus(p_settlements, "figs/fig01A_settlements_double.pdf",
#                 column = "double", height_mm = 120)

# Single-column master (90 mm)
# ggsave_pdf_ceus(p_sample, "figs/fig01B_sample_single.pdf",
#                 column = "single", height_mm = 110)
# Double-column master (190 mm)
# ggsave_pdf_ceus(p_sample, "figs/fig01B_sample_double.pdf",
#                 column = "double", height_mm = 120)

# Raster copies (Elsevier: combo 500 dpi)
ggsave_tiff_ceus(p_settlements, "figs/fig01A_settlements_single_combo.tiff",
                 column = "single", height_mm = 110, type = "combo")

# ggsave_pdf_ceus(p_sample, "figs/fig01B_sample_single.pdf",
#                 column = "single", height_mm = 110)
# ggsave_pdf_ceus(p_sample, "figs/fig01B_sample_double.pdf",
#                 column = "double", height_mm = 110)
# Export rasters as well
ggsave_tiff_ceus(p_sample, "figs/fig01A_sample_single_combo.tiff",
                 column = "single", height_mm = 110, type = "combo")

# 6) Composite A+B
compose_ab_ceus(
  pA = p_settlements, pB = p_sample,
  file = "figs/fig01_study-area_AB_single.pdf",
  column = "single", height_mm = 110,
  tags = c("A", "B"), legends = c("bottom", "bottom")
)

compose_ab_ceus(
  pA = p_settlements, pB = p_sample,
  file = "figs/fig01_study-area_AB_double.pdf",
  column = "double", height_mm = 120,
  tags = c("A", "B"), legends = c("bottom", "bottom")
)
