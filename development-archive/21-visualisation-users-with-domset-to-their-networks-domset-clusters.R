# =============================================================================
# Development script - not part of the final analysis
#
# Settlement composition of contacts by user cluster; the cluster-based
# forerunner of script 13.
#
# Kept as a record of how the analysis developed. The final pipeline is
# in scripts/; see README.md.
# =============================================================================

library(tidyverse)

# Build the data (totals, member groups, alter mix) --------------------------

# --- Inputes ---
# clusters: user_id, cg_domset (dominant settlement), cluster (1..4)
# edges_pass: caller, callee

edges_raw <- if (exists("edges_pass")) edges_pass else edges_pass

# Standardise labels
lab_map <- c("Urban: big city"   = "Big city",
             "Urban: small city" = "Small city",
             "rural"             = "Rural")

lev <- c("Big city", "Small city", "Rural")

clusters <- DT %>%
  select(user_id, cg_domset, cluster)

# nodes: clusters data -> id, dom (factor), cl (1..4)
nodes <- clusters %>%
  transmute(id = as.character(user_id),
            dom = factor(lab_map[as.character(cg_domset)], levels = lev),
            cl = as.integer(cluster)) %>%
  filter(!is.na(dom), !is.na(cl))

# undirected, de-duplicated pairs (one row per pair) ---
E <- edges_raw %>%
  transmute(u = as.character(caller),
            v = as.character(callee)) %>%
  filter(!is.na(u), !is.na(v), u != v) %>%
  mutate(a = pmin(u, v), b = pmax(u, v)) %>%
  distinct(a, b)

# attach attributes to both nodes
P <- E %>%
  left_join(nodes, by = c("a" = "id")) %>% rename(dom_a = dom, cl_a = cl) %>%
  left_join(nodes, by = c("b" = "id")) %>% rename(dom_b = dom, cl_b = cl)

# long view: two directions; keep only rows with known dom & cluster
LONG <- bind_rows(
  transmute(P, ego = a, alter = b, ego_dom = dom_a, alter_dom = dom_b, cl = cl_a),
  transmute(P, ego = b, alter = a, ego_dom = dom_b, alter_dom = dom_a, cl = cl_b)
) %>%
  filter(!is.na(ego_dom), !is.na(alter_dom), !is.na(cl))

# --- totals per cluster ---
totals <- nodes %>%
  count(cl, name = "n_members") %>%
  mutate(cluster = paste("Profile", cl))

# --- members per cluster x ego_dom ---
members_by_dom <- nodes %>%
  count(cl, ego_dom = dom, name = "n_users") %>%
  complete(cl, ego_dom = factor(lev, levels = lev), fill = list(n_users = 0L)) %>%
  mutate(cluster = paste("Profile", cl))

# --- unique-alter composition per cluster x ego_dom x alter_dom ---
alters_mix <- LONG %>%
  distinct(cl, ego_dom, alter, .keep_all = TRUE) %>%    # unique alters
  count(cl, ego_dom, alter_dom, name = "n_alters") %>%
  complete(cl,
           ego_dom   = factor(lev, levels = lev),
           alter_dom = factor(lev, levels = lev),
           fill = list(n_alters = 0L)) %>%
  group_by(cl, ego_dom) %>%
  mutate(n_alters_tot = sum(n_alters),
         share = ifelse(n_alters_tot > 0, n_alters / n_alters_tot, 0)) %>%
  ungroup() %>%
  mutate(cluster = paste("Profile", cl))

# Lay out a vertical 3-level flow and draw -----------------------------------

# positions (same for all facets)
# y=3 : cluster total
# y=2 : member groups (Big/Small/Rural)
# y=1 : alter groups under each member group

x_mid <- c("Big city"=1, "Small city"=3, "Rural"=5)
offs_bot  <- c("Big city"=-0.7, "Small city"=0, "Rural"=+0.7)

# top node (cluster total)
top_nodes <- totals %>%
  transmute(
    cluster, 
    x=3, 
    y=4,
    n_members,
    size_top = res(n_members, 9, 18),
    label = paste0("Users: ", format(n_members, big.mark = " "))
  )

# member-group nodes
rng <- range(members_by_dom$n_users)
res <- function(x, a, b) if (diff(rng)==0) mean(c(a,b)) else (x - rng[1]) / diff(rng) * (b - a) + a

mid_nodes <- members_by_dom %>%
  transmute(
    cluster,
    ego_dom,
    x = unname(x_mid[as.character(ego_dom)]),
    y = 2.5,
    n_users,
    size_mid = res(n_users, 8, 18)   # bubble size for member groups
  )

# alter-group nodes (under each mid node)
bot_nodes <- alters_mix %>%
  transmute(
    cluster, ego_dom, alter_dom,
    x = unname(x_mid[as.character(ego_dom)]) + unname(offs_bot[as.character(alter_dom)]),
    y = 0.6,
    share, n_alters,
    size_bot = 1.5 + 9*sqrt(pmax(share,0)),    # 6 + 18*sqrt(pmax(share,0))   res(n_alters, 2, 10)
    pct = paste0(round(100*share), "%")
  )

# edges: top -> mid (width by n_users / cluster max)
e1 <- members_by_dom %>%
  group_by(cluster) %>% mutate(maxu = max(n_users)) %>% ungroup() %>%
  transmute(cluster,
            x = 3, y = 4,
            xend = unname(x_mid[as.character(ego_dom)]), yend = 2.5,
            lw1 = 0.6 + 4 * ifelse(maxu == 0, 0, n_users / maxu))

# edges: mid -> bot (width by share)
bot_nodes2 <- bot_nodes %>% rename(x_end = x, y_end = y)

e2 <- bot_nodes2 %>%
  transmute(cluster,
            x = unname(x_mid[as.character(ego_dom)]), y = 2.5,
            xend = x_end, yend = y_end,
            lw2 = 0.5 + 4 * share, alter_dom)

# titles for each facet
titles <- data.frame(cluster = unique(totals$cluster))

# Plot -----------------------------------------------------------------------

# palette
pal3 <- c("Big city"   = "#122C43",
          "Small city" = "#C38F16",
          "Rural"      = "#2C6B67")

# --- plot (4 columns) ---
p_flow <- ggplot() +

  # flows first
  geom_curve(data = e1,
             aes(x = x, y = y, xend = xend, yend = yend, linewidth = lw1),
             curvature = -0.2, colour = "#d9d9d9", lineend = "round", show.legend = FALSE) +
  scale_linewidth_identity() +

  geom_curve(data = e2,
             aes(x = x, y = y, xend = xend, yend = yend, linewidth = lw2),
             curvature = -0.25, colour = "#d9d9d9", lineend = "round", show.legend = FALSE) +

  # bottom (alter) bubbles
  geom_point(data = bot_nodes,
             aes(x = x, y = y, fill = alter_dom, size = size_bot),
             shape = 21, colour = "#d9d9d9", stroke = 0.01, show.legend = TRUE) +
  scale_fill_manual(values = pal3, name = "Dominant settlement") +
  scale_size_identity(guide = "none") +
  geom_text(data = bot_nodes,
            aes(x = x, y = y - 0.08, label = pct), colour = "#737373",
            vjust = 3, size = 7, size.unit = "pt") +

  # mid (member groups)
  geom_point(data = mid_nodes,
             aes(x = x, y = y, size = size_mid, fill = ego_dom),
             shape = 21, colour = "#d9d9d9", stroke = 0.8, show.legend = FALSE) +
  #scale_size_identity() +

  #geom_text(data = mid_nodes,
  #aes(x = x, y = y + 0.28, label = as.character(ego_dom)), colour = "#525252",
  #fontface = "bold", vjust = 0, size = 7, size.unit = "pt") +
  geom_text(data = mid_nodes,
            aes(x = x, y = y + 0.10, label = format(n_users, big.mark = " ")), colour = "#525252",
            vjust = 2, hjust = -0.8, size = 7, size.unit = "pt") +

  # top (cluster total)
  geom_point(data = top_nodes, 
             aes(x = x, y = y, size = size_top), fill = "#d9d9d9", colour = "#bdbdbd",
             shape = 21, stroke = 0.6, show.legend = FALSE) +   # fill = ego_dom, colour = ego_dom,  
  #scale_size(range = c(12, 26), guide = "none") +
  #scale_size_identity() +
  geom_text(data = titles,
            aes(x = 3, y = 4, label = cluster), colour = "#252525",
            fontface = "bold", vjust = -5.5, size = 12, size.unit = "pt") +
  geom_text(data = top_nodes,
            aes(x = x, y = y - 0.22, label = label), colour = "#525252", 
            vjust = -7.5, size = 8, size.unit = "pt") +

  facet_wrap(~cluster, ncol = 2) +

  coord_fixed(ratio = 1, 
              xlim = c(0, 6), ylim = c(0.25, 5.5)) +  # , xlim = c(0.1, 3.9), ylim = c(-0.9, 4.2)

  theme_minimal(base_family = "sans") +

  theme(
    #plot.title         = element_text(),
    #plot.subtitle      = element_text(),
    #plot.caption       = element_text("),
    #plot.tag           = element_text(),
    #plot.tag.position  = c(),

    axis.title = element_blank(),
    axis.text  = element_blank(),

    #strip.background = element_rect(fill = "#d9d9d9"),
    strip.text = element_blank(),  #  element_text(face =  "bold.italic", size = 12, vjust = -1) , vjust = 0

    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),

    legend.position = "bottom",
    legend.box.background = element_blank(),

    #legend.box.margin = margin(t = -4, r = 0, b = 0, l =0),
    legend.margin = margin(t = 0, r = 0, b = -3, l =0),
    #legend.spacing.y = unit(0, "mm"),

    legend.title       = element_text(size = 8, colour = "#252525"),
    legend.text        = element_text(size = 7, colour = "#525252"),
    legend.key.height  = unit(6.5, "mm"),
    legend.key.width   = unit(8.0, "mm"),

    plot.background = element_rect(fill = "#F9F9EB", colour = NA),

    plot.margin        = margin(8, 0, 8, 0),  # mm around the figure

    panel.spacing.x = unit(4, "mm"),
    panel.spacing.y = unit(6, "mm")
    #plot.margin     = margin(4, 6, 4, 6)
  )

p_flow

# TIFF @ 1000 dpi
ggsave("figs/p_flow.tiff", p_flow, device = "tiff",
       width = 190, height = 165, units = "mm",
       dpi = 1000, compression = "lzw")
