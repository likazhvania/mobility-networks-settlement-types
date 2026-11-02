# =============================================================================
# Development script - not part of the final analysis
#
# Theme, palette and export helpers for figures.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

# This file defines:

# Theme templates tuned to CEUS column widths (single 90 mm, 1.5-col 140 mm,
# double 190 mm) Lbael & palette helpers (Okabe-Ito categorical; sane
# sequential ramps) Panel tags (A/B/C) without ggrepel Export helpers for PDF
# (vector) and TIFF/PNG (raste) in CEUS sizes, using Cairo PDF if available

# --- 0) Setup ---
library(sf)
library(ggplot2)

# CEUS (Elsevier) column widths in millimetres
CEUS_WIDTHS_MM <- list(
  single  = 90,   # single-column
  onehalf = 140,  # 1.5-column (handy for slightly wider maps)
  double  = 190   # double-column
)

# Safe PDF device: prefer Cairo for embedded fonts; fallback to pdf()
ceus_pdf_device <- function() {
  if (capabilities("cairo")) cairo_pdf else pdf
}

# Helper: map "single"|"onehalf"|"double" -> width in mm
ceus_width_mm <- function(column = c("single", "onehalf", "double")) {
  column <- match.arg(column)
  CEUS_WIDTHS_MM[[column]]
}

# Theme templates ------------------------------------------------------------

# Base figure theme tuned per column width
theme_fig_ceus <- function(column = c("single", "onehalf", "double"),
                           base_family = "sans") {
  column <- match.arg(column)
  # Font sizing targets (points) per width
  if (column == "single") {
    base_size <- 8   # core text ~8 pt
    title_pt  <- 9.5
    tag_pt    <- 9.5
  } else if (column == "onehalf") {
    base_size <- 8.5
    title_pt  <- 10.5
    tag_pt    <- 10.0
  } else { # double
    base_size <- 9.0
    title_pt  <- 11.0
    tag_pt    <- 10.5
  }

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title         = element_text(face = "bold", size = title_pt),
      plot.subtitle      = element_text(size = base_size),
      plot.caption       = element_text(size = base_size - 1, colour = "grey30"),
      plot.tag           = element_text(face = "bold", size = tag_pt),
      plot.tag.position  = c(0.005, 0.99),

      axis.title         = element_text(size = base_size),
      axis.text          = element_text(size = base_size - 0.5, colour = "grey20"),

      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(linewidth = 0.25, colour = "grey90"),

      legend.title       = element_text(size = base_size),
      legend.text        = element_text(size = base_size - 0.5),
      legend.key.height  = unit(3.5, "mm"),
      legend.key.width   = unit(5.0, "mm"),

      plot.margin        = margin(6, 6, 6, 6)  # mm around the figure
    )
}

# A variant for maps (axes and grids toned down)
theme_map_ceus <- function(column = c("single", "onehalf", "double"),
                           base_family = "sans",
                           legend_position = "right") {
  theme_fig_ceus(column = column, base_family = base_family) +
    theme(
      axis.title       = element_blank(),
      panel.grid.major = element_blank(),
      axis.text        = element_blank(),
      legend.position  = legend_position
    )
}

# Label and Palette helpers --------------------------------------------------

# Okabe-Ito categorical palette (colour-blind safe)
oi_palette <- c(
  "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7", "#000000"
)

scale_fill_oi <- function(..., n = NULL) {
  values <- if (is.null(n)) oi_palette else oi_palette[seq_len(n)]
  scale_fill_manual(values = values, ...)
}

scale_colour_oi <- function(..., n = NULL) {
  values <- if (is.null(n)) oi_palette else oi_palette[seq_len(n)]
  scale_colour_manual(values = values, ...)
}

# Sequential ramps (no extra packages)
ramp_blues   <- grDevices::colorRampPalette(c("#f7fbff", "#deebf7", "#c6dbef",
                                              "#9ecae1", "#6baed6", "#3182bd", "#08519c"))

ramp_greens  <- grDevices::colorRampPalette(c("#f7fcf5", "#e5f5e0", "#c7e9c0",
                                              "#a1d99b", "#74c476", "#31a354", "#006d2c"))

ramp_greys   <- grDevices::colorRampPalette(c("#ffffff", "#f0f0f0", "#d9d9d9",
                                              "#bdbdbd", "#969696", "#737373", "#525252"))

# Convenient scales for choropleths
scale_fill_seq_blues <- function(n = 7, ...) scale_fill_gradientn(colours = ramp_blues(n), ...)
scale_fill_seq_green <- function(n = 7, ...) scale_fill_gradientn(colours = ramp_greens(n), ...)
scale_fill_seq_grey  <- function(n = 7, ...) scale_fill_gradientn(colours = ramp_greys(n), ...)

# Common label helpers (scales is already imported by ggplot2)
lab_comma   <- scales::label_comma()
lab_percent <- function(accuracy = 1) scales::label_percent(accuracy = accuracy)
lab_num     <- function(accuracy = 0.1) scales::label_number(accuracy = accuracy)

# Panel tags & Plot labels ---------------------------------------------------

# Add a bold "A/B/C" tag using ggplot's built-in plot.tag
add_panel_tag <- function(p, tag = "A") {
  p + labs(tag = tag)
}

# Title/Subtitle/Caption shortcut (keeps the code tidy)
labs_fig <- function(title = NULL, subtitle = NULL, caption = NULL) {
  labs(title = title, subtitle = subtitle, caption = caption)
}

# Export helpers (PDF + raster) for CEUS sizes -------------------------------

# Internal: choose width from "single"/"onehalf"/"double"
.resolve_width <- function(column) {
  ceus_width_mm(column)
}

# 4.1 Vector PDF export (fonts embedded via Cairo if available)
ggsave_pdf_ceus <- function(p, file,
                            column = c("single", "onehalf", "double"),
                            height_mm = 110,
                            use_dingbats = FALSE) {
  column <- match.arg(column)
  width_mm <- .resolve_width(column)
  device_fun <- ceus_pdf_device()
  ggsave(
    filename = file, plot = p,
    width = width_mm, height = height_mm, units = "mm",
    device = device_fun, dpi = 300, useDingbats = use_dingbats
  )
  invisible(file)
}

# 4.2 Raster TIFF export (CEUS/Elsevier typical dpi: line=1000, combo=500, photo=300)
# type = "line" (pure line art), "combo" (mixed), "photo" (image-heavy)
ggsave_tiff_ceus <- function(p, file,
                             column = c("single", "onehalf", "double"),
                             height_mm = 110,
                             type = c("combo", "line", "photo")) {
  column <- match.arg(column)
  type <- match.arg(type)
  dpi <- switch(type, line = 1000, combo = 500, photo = 300)
  width_mm <- .resolve_width(column)
  ggsave(
    filename = file, plot = p,
    width = width_mm, height = height_mm, units = "mm",
    dpi = dpi, device = "tiff", compression = "lzw"
  )
  invisible(file)
}

# 4.3 Raster PNG export (journals rarely demand it, but useful for Word/Slides)
ggsave_png_ceus <- function(p, file,
                            column = c("single", "onehalf", "double"),
                            height_mm = 110, dpi = 600) {
  column <- match.arg(column)
  width_mm <- .resolve_width(column)

  ggsave(
    filename = file, plot = p,
    width = width_mm, height = height_mm, units = "mm",
    dpi = dpi, device = "png"
  )
  invisible(file)
}

# Add the compositor ---------------------------------------------------------

# --- Panel compositor (A/B) without extra packages ---
compose_ab_ceus <- function(pA, pB, file,
                            column = c("single", "onehalf", "double"),
                            height_mm = 120,
                            tags = c("A", "B"),
                            legends = c("bottom", "bottom")) {

  column <- match.arg(column)
  width_mm <- ceus_width_mm(column)
  device_fun <- ceus_pdf_device()

  get_legend <- function(p) {
    g <- ggplotGrob(p + theme(legend.position = "bottom"))
    idx <- which(g$layout$name == "guide-box")
    if (length(idx)) g$grobs[[idx]] else NULL
  }

  legA <- if (legends[1] == "bottom") get_legend(pA) else NULL
  legB <- if (legends[2] == "bottom") get_legend(pB) else NULL

  pA <- pA + labs(tag = tags[1]) + theme(legend.position = "none")
  pB <- pB + labs(tag = tags[2]) + theme(legend.position = "none")

  device_fun(file = file, width = width_mm/25.4, height = height_mm/25.4)
  on.exit(dev.off(), add = TRUE)

  grid::grid.newpage()

  h_leg <- grid::unit(0, "mm")
  if (!is.null(legA)) h_leg <- max(h_leg, grid::grobHeight(legA))
  if (!is.null(legB)) h_leg <- max(h_leg, grid::grobHeight(legB))
  lay <- grid::grid.layout(
    nrow = 2, ncol = 2,
    widths  = grid::unit.c(grid::unit(1, "null"), grid::unit(1, "null")),
    heights = grid::unit.c(grid::unit(1, "null"), h_leg + grid::unit(2, "mm"))
  )

  grid::pushViewport(grid::viewport(layout = lay))

  print(pA, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(pB, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))

  if (!is.null(legA)) { grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1)); grid::grid.draw(legA); grid::upViewport() }
  if (!is.null(legB)) { grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 2)); grid::grid.draw(legB); grid::upViewport() }

  invisible(file)
}

# Minimal usage examples (safe to delete later) ------------------------------

# Example (replace with real map)
ex_df <- data.frame(x = rnorm(400), y = rnorm(400))
p_demo <- ggplot(ex_df, aes(x, y)) +
  geom_point(alpha = 0.6, size = 0.6, colour = "#0072B2") +
  theme_fig_ceus("single") +
  labs_fig("Demo figure", "Replace with real map", "CEUS-ready helper") +
  add_panel_tag("A")

# Export at CEUS single & double widths (adjust height_mm as needed)

# Vector
ggsave_pdf_ceus(p_demo, "figs/_demo_single.pdf", column = "single", height_mm = 110)
ggsave_pdf_ceus(p_demo, "figs/_demo_double.pdf", column = "double", height_mm = 120)

# Raster
ggsave_tiff_ceus(p_demo, "figs/_demo_single_combo.tiff", column = "single", type = "combo")
ggsave_png_ceus(p_demo, "figs/_demo_single_600.png", column = "single", dpi = 600)
