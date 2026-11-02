# =============================================================================
# 13  Settlement composition of contacts
#
# Users are grouped by their dominant settlement type (cg_domset: the
# settlement type from which they send most of their events). For each group,
# the distinct contacts of all its members are pooled, and the shares of
# those contacts by their own dominant settlement type are calculated. The
# result is drawn as a bubble diagram: one bubble per user group at the top,
# and the settlement composition of its contacts below.
#
# Input:  analysis_data (script 08): user_id, cg_domset
#         edges_pass (script 03): caller, callee
# Output: shares_unique - contact composition per group; figure p_mix
# =============================================================================

library(tidyverse)

# Contact composition per group ----------------------------------------------

# --- 0) Columns needed ---
users_domset <- analysis_data %>%
  transmute(id = as.character(user_id),
            dom = cg_domset) %>%
  filter(!is.na(dom))

edges_raw <- if (exists("edges_pass")) edges_pass else edges_pass
edges_raw <- edges_raw %>%
  transmute(u = as.character(caller),
            v = as.character(callee)) %>%
  filter(!is.na(u), !is.na(v), u != v)

# --- 1) Undirected pairs, one row per pair ---

E <- edges_raw %>%
  mutate(a = pmin(u, v), b = pmax(u, v)) %>%
  distinct(a, b)

# --- 2) Dominant settlement of both users; pairs with a missing one dropped ---
pairs <- E %>%
  left_join(users_domset, by = c("a" = "id")) %>% rename(dom_a = dom) %>%
  left_join(users_domset, by = c("b" = "id")) %>% rename(dom_b = dom) %>%
  filter(!is.na(dom_a), !is.na(dom_b))

# --- 3) Each pair seen from both sides: one row per ego -> alter ---
long <- bind_rows(
  transmute(pairs, ego = a, ego_dom = dom_a, alter = b, alter_dom = dom_b),
  transmute(pairs, ego = b, ego_dom = dom_b, alter = a, alter_dom = dom_a)
)

# --- 4) Composition of distinct contacts per group ---
# For each group (big city, small city, rural), every contact of its members
# is counted once, however many members are linked to them. The shares are
# the proportions of these contacts by their dominant settlement type.
shares_unique <- long %>%
  distinct(ego_dom, alter, .keep_all = TRUE) %>%     # each contact once per group
  count(ego_dom, alter_dom, name = "n_alters") %>%
  group_by(ego_dom) %>%
  mutate(share = n_alters / sum(n_alters)) %>%
  ungroup() %>%
  arrange(ego_dom, desc(share))

# --- 5) Variant: contacts outside the user's own group only ---
# Exploratory; not used in the study.

# shares_unique_outside <- long %>%
#   filter(alter_dom != ego_dom) %>%
#   distinct(ego_dom, alter, .keep_all = TRUE) %>%
#   count(ego_dom, alter_dom, name = "n_alters") %>%
#   group_by(ego_dom) %>%
#   mutate(share = n_alters / sum(n_alters)) %>%
#   ungroup() %>%
#   arrange(ego_dom, desc(share))

# Figure ---------------------------------------------------------------------

# Uses users_domset (id, dom) and shares_unique (ego_dom, alter_dom, n_alters,
# share) from above.

# --- 0) Short labels in a fixed order ---
lab_map <- c("Urban: big city"   = "Big city",
             "Urban: small city" = "Small city",
             "rural"             = "Rural")

lev <- c("Big city", "Small city", "Rural")

users_domset2 <- users_domset %>%
  transmute(id,
            dom = factor(lab_map[as.character(dom)], levels = lev))

shares2 <- shares_unique %>%
  transmute(ego_dom   = factor(lab_map[as.character(ego_dom)],   levels = lev),
            alter_dom = factor(lab_map[as.character(alter_dom)], levels = lev),
            n_alters, share)

# --- 1) Users per group and distinct contacts per group ---
group_sizes <- users_domset2 %>% count(dom, name = "n_users") %>% rename(ego_dom = dom)
alter_totals <- shares2 %>% group_by(ego_dom) %>% summarise(n_alters_tot = sum(n_alters), .groups = "drop")

# --- 2) Layout: three columns (big city, small city, rural), two rows ---
col_pos <- data.frame(
  ego_dom = factor(lev, levels = lev),
  x_top = c(1, 2, 3),
  y_top = 1
)

# Horizontal offsets of the three contact bubbles under each group bubble
offs <- c("Big city"=-0.35, "Small city"=0, "Rural"=0.35)

bottom_nodes <- shares2 %>%
  left_join(col_pos, by = "ego_dom") %>%
  mutate(x = x_top + offs[as.character(alter_dom)],
         y = 0,
         # Bubble size grows with the square root of the share, so that area
         # roughly follows the share
         pt_size = 5 + 20*sqrt(pmax(share,0)),        # minimum 5, scaling factor 20
         pct_lab = paste0(round(100*share), "%"))

top_nodes <- group_sizes %>%
  left_join(alter_totals, by = "ego_dom") %>%
  left_join(col_pos, by = "ego_dom") %>%
  mutate(
    # Group bubble size: number of users, rescaled linearly to 12-28 pt
    top_size = {
      x <- n_users; if (max(x)==min(x)) rep(20,length(x))
      else (x-min(x))/(max(x)-min(x))*(28-12)+12
    },
    title = as.character(ego_dom),
    subtitle = paste0("Users: ", format(n_users, big.mark=" "),
                      "\nAlters: ", format(n_alters_tot, big.mark=" "))
  )

# Colours for the contact groups, matching the maps. Three versions were
# tried; the last definition is the one used.
pal3 <- c("Big city"   = "#0A2E57",
          "Small city" = "#5A97C1",
          "Rural"      = "#95C36E")

pal3 <- c("Big city"   = "#122C43",
          "Small city" = "#C38F16",
          "Rural"      = "#2C6B67")

pal3 <- c("Big city"   = "#0A2E57",
          "Small city" = "#C38F16",
          "Rural"      = "#2C6B67")

# --- 3) Arcs from group bubbles to contact bubbles (width by share) ---
edges <- bottom_nodes %>%
  rename(xend = x, yend = y) %>%
  select(ego_dom, alter_dom, x_top, y_top, xend, yend, share) %>%
  mutate(lw = 0.5 + 3*share)   # line width

xmin <- min(c(bottom_nodes$x, top_nodes$x_top)) - 0.1
xmax <- max(c(bottom_nodes$x, top_nodes$x_top)) + 0.1
ymin <- min(bottom_nodes$y) - 0.12    # space below the lower bubbles
#ymin <- ymin + 0.04
ymax <- max(top_nodes$y_top) + 0.18   # space above the upper bubbles

# --- 4) Plot ---
p_mix <- ggplot() +

  # Arcs
  geom_curve(data = edges,
             aes(x = x_top, y = y_top, xend = xend, yend = yend, linewidth = lw),
             curvature = -0.35, lineend = "round", colour = "#d9d9d9", 
             show.legend = FALSE) +
  scale_linewidth_identity() +       # line widths taken as given in lw; no legend

  # Lower bubbles: composition of contacts
  geom_point(data = bottom_nodes,
             aes(x = x, y = y, fill = alter_dom, size = pt_size),
             shape = 21, colour = "#d9d9d9", stroke = 0.01,
             show.legend = TRUE) +
  scale_size_identity(guide = "none") +   # sizes taken as given; no legend
  geom_text(data = bottom_nodes,
            aes(x = x, y = y - 0.08, label = pct_lab), colour = "#525252",
            vjust = 3.5, size = 7, size.unit = "pt") +

  # Upper bubbles: user groups
  geom_point(data = top_nodes,
             aes(x = x_top, y = y_top, size = top_size),
             shape = 21, fill = "#d9d9d9", colour = "#bdbdbd", stroke = 0.6,
             show.legend = FALSE) +
  # scale_size_identity() +      # (keeps previous 'identity'; if already added above, this is optional)
  geom_text(data = top_nodes,
            aes(x = x_top, y = y_top, label = title), colour = "#252525",
            fontface = "bold", vjust = -5.5, size = 10, size.unit = "pt") + 

  geom_text(data = top_nodes,
            aes(x = x_top, y = y_top, label = subtitle), colour = "#525252",
            vjust = 2.9, hjust = 1.3, size = 8, size.unit = "pt") +

  scale_fill_manual(values = pal3, name = "Alter dominant settlement") +
  #scale_y_continuous(limits = c(-0.55, 1.35), expand = expansion(mult = c(0.05, 0.05))) +
  #scale_x_continuous(limits = c(0.3, 3.7)) +
  coord_fixed(ratio = 1,
              xlim = c(xmin, xmax), ylim = c(ymin, ymax), clip = "off") + 

  theme_minimal(base_family = "sans") +

  theme(
    #plot.title         = element_text(),
    #plot.subtitle      = element_text(),
    #plot.caption       = element_text("),
    #plot.tag           = element_text(),
    #plot.tag.position  = c(),

    axis.title = element_blank(),
    axis.text  = element_blank(),

    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),

    legend.position = "bottom",
    legend.box.background = element_blank(),

    #legend.box.margin = margin(t = 0, r = 0, b = 2, l =0),
    legend.margin = margin(t = 0, r = 0, b = -3, l =0),
    #legend.spacing.y = unit(0, "mm"),

    legend.title       = element_text(size = 8, colour = "#252525"),
    legend.text        = element_text(size = 7, colour = "#525252"),
    legend.key.height  = unit(6.5, "mm"),
    legend.key.width   = unit(8.0, "mm"),

    plot.background = element_rect(fill = "#F9F9EB", colour = NA),

    plot.margin        = margin(6, 8, 2, 8)  # top, right, bottom, left, in points
  )

#+
#labs(title = "Users by dominant settlement and the composition of their unique alters")

p_mix

# Export ---------------------------------------------------------------------

# Double-column width (183 mm) in four formats.

# PDF
ggsave("figs/V3.pdf", p_mix, device = cairo_pdf,
       width = 183, height = 100, units = "mm",
       dpi = 300)

# EPS
ggsave("figs/V3.eps", p_mix, device = cairo_ps,
       width = 183, height = 100, units = "mm",
       onefile = FALSE, fallback_resolution = 1200)

# TIFF @ 1000 dpi
ggsave("figs/V3.tiff", p_mix, device = "tiff",
       width = 183, height = 100, units = "mm",
       dpi = 1000, compression = "lzw")

# JPEG
ggsave("figs/V3.jpg", p_mix, device = "jpeg",
       width = 183, height = 100, units = "mm",
       dpi = 600, quality = 100)
