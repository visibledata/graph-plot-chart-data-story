library("tidyverse")
library("GPCDStools")
library("patchwork")
library("cjhRutils")
library("ggtext")
library("ggforce")

cols_gpcds <- as.list(deframe(select(colours_gpcds, name, hex_code)))

ident_linewidth <- 5
ident_disk_size <- 25
ident_textsize <- 40
ident_minor_disk_factor <- sqrt(5)
ident_font <- "Futura"

graph_x_first <- 12.5
graph_y_first <- 50
graph_y_height <- 30

graph_line_1_slope <- tan(60 * pi / 180)
graph_line_2_slope <- tan(-60 * pi / 180)

graph_line_1_intercept <- - {50 + graph_line_1_slope * 50}
graph_line_2_intercept <- - {50 + graph_line_2_slope * {100-graph_x_first}}

graph_bottom_disk_x = {graph_line_2_intercept - graph_line_1_intercept} / {graph_line_1_slope - graph_line_2_slope}

graph_bottom_disk_y = - {graph_line_1_slope * graph_bottom_disk_x + graph_line_1_intercept}

gg_graph_ident_major <- tibble(
  x = c(graph_x_first, 100 - graph_x_first, 50, graph_bottom_disk_x),
  y = c(graph_y_first, graph_y_first, graph_y_first, graph_bottom_disk_y),
  # colour = "secondary"
  colour = c("secondary", "tertiary", "tertiary", "secondary")
) %>%
  ggplot() +
  annotate(
    "segment",
    x = graph_x_first, 
    xend = 100 - graph_x_first,
    y = graph_y_first, 
    yend = graph_y_first,
    linewidth = ident_linewidth,
    colour = cols_gpcds$graph_tertiary_darker) +
  annotate(
    "segment",
    x = c(50, 100 - graph_x_first), 
    xend = c(graph_bottom_disk_x, graph_bottom_disk_x),
    y = c(graph_y_first, graph_y_first), 
    yend = c(graph_bottom_disk_y, graph_bottom_disk_y),
    linewidth = ident_linewidth,
    colour = cols_gpcds$graph_tertiary_darker) +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size,
             show.legend = FALSE) +
  geom_text(
    x = 50,
    y = 70,
    label = "GRAPH",
    size = ident_textsize,
    colour = "white",
    family = "Futura",
    lineheight = 0.9
  ) +
  scale_colour_manual(
    values = c(
      "secondary" = cols_gpcds$graph_secondary,
      "tertiary" = cols_gpcds$graph_tertiary
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = cols_gpcds$graph_primary,
      colour = cols_gpcds$graph_primary
    )
  )

ggsave("brand-identity/ident_major_graph.png",
       gg_graph_ident_major,
       width = 8,
       height = 8)

graph_minor_x_first <- 20
graph_y_first <- 50
graph_y_height <- 30


gg_graph_ident_minor <- tibble(
  x = c(graph_minor_x_first, 100 - graph_minor_x_first),
  y = c(graph_y_first, graph_y_first),
  # colour = "secondary"
  colour = c("secondary", "tertiary")
) %>%
  ggplot() +
  annotate(
    "segment",
    x = graph_minor_x_first, 
    xend = 100 - graph_minor_x_first,
    y = graph_y_first, 
    yend = graph_y_first,
    linewidth = ident_linewidth + 2,
    colour = cols_gpcds$graph_tertiary_darker) +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size * ident_minor_disk_factor,
             show.legend = FALSE) +
  scale_colour_manual(
    values = c(
      "secondary" = cols_gpcds$graph_secondary,
      "tertiary" = cols_gpcds$graph_tertiary
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = cols_gpcds$graph_primary,
      colour = cols_gpcds$graph_primary
    )
  )

ggsave("brand-identity/ident_minor_graph.png",
       gg_graph_ident_minor,
       width = 8,
       height = 8)


# Plot Idents -------------------------------------------------------------

gg_plot_ident_major <- tibble(
  x = c(12.5, 37.5, 62.5, 87.5),
  y = rev(c(12.5, 37.5, 62.5, 87.5)),
  colour = c("tertiary", "tertiary", "secondary", "secondary")
) %>% 
  ggplot() +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size,
             show.legend = FALSE) +
  geom_text(
    x = 75,
    y = 62.5,
    label = "PLOT",
    size = ident_textsize,
    colour = "white",
    family = "Futura") +
  geom_vline(xintercept = 50,
             colour = cols_gpcds$plot_tertiary_darker,
             linewidth = ident_linewidth) +
  geom_hline(yintercept = 50,
             colour = cols_gpcds$plot_tertiary_darker,
             linewidth = ident_linewidth) +
  scale_colour_manual(values = c("secondary" = cols_gpcds$plot_secondary,
                                 "tertiary" = cols_gpcds$plot_tertiary)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100),
              ylim = c(0, 100)) +
  theme_void() +
  theme(panel.background = element_rect(fill = cols_gpcds$plot_primary,
                                        colour = cols_gpcds$plot_primary))

ggsave("brand-identity/ident_major_plot.png",
       gg_plot_ident_major,
       width = 8,
       height = 8)

gg_plot_ident_minor <- tibble(
  x = c(30, 70),
  y = rev(c(30, 70)),
  colour = c("tertiary", "secondary")
) %>% 
  ggplot() +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size * ident_minor_disk_factor,
             show.legend = FALSE) +
  scale_colour_manual(values = c("secondary" = cols_gpcds$plot_secondary,
                                 "tertiary" = cols_gpcds$plot_tertiary)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100),
              ylim = c(0, 100)) +
  theme_void() +
  theme(panel.background = element_rect(fill = cols_gpcds$plot_primary,
                                        colour = cols_gpcds$plot_primary))

ggsave("brand-identity/ident_minor_plot.png",
       gg_plot_ident_minor,
       width = 8,
       height = 8)


# Chart idents ------------------------------------------------------------

chart_center_circle_coords <- list(x = 50, y = 50)
chart_center_circle_radius <- 37.5

# Solve (x - 50)² + (y - 50)² = 37.5² and y = x
chart_equ_solution <- 50 - 75 / {2 * sqrt(2)}

gg_chart_ident_major <- tibble(
  x = c(chart_equ_solution, 100-chart_equ_solution, 100 - 12.5, chart_equ_solution),
  y = c(100-chart_equ_solution, chart_equ_solution, 100 - 12.5, chart_equ_solution),
  colour = c("tertiary", "tertiary", "secondary", "secondary")
) %>%
  ggplot() +
  geom_circle(data = tibble(),
              aes(x0 = 50, y0 = 50, r = chart_center_circle_radius),
              linewidth = ident_linewidth,
              colour = cols_gpcds$chart_tertiary_darker) +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size,
             show.legend = FALSE) +
  geom_text(
    x = 50,
    y = 50,
    label = "CHART",
    size = ident_textsize,
    colour = "white",
    family = "Futura"
  ) +
  scale_colour_manual(
    values = c(
      "secondary" = cols_gpcds$chart_secondary,
      "tertiary" = cols_gpcds$chart_tertiary
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = cols_gpcds$chart_primary,
      colour = cols_gpcds$chart_primary
    )
  )

ggsave("brand-identity/ident_major_chart.png",
       gg_chart_ident_major,
       width = 8,
       height = 8)

chart_minor_center_circle_radius <- 37.5

# Solve (x - 50)² + (y - 50)² = 37.5² and y = x
chart_equ_solution <- 50 - 75 / {2 * sqrt(2)}

gg_chart_ident_minor <- tibble(
  x = 100 - chart_equ_solution,
  y = 100 - chart_equ_solution,
  colour = "secondary"
) %>%
  ggplot() +
  
  geom_circle(aes(x0 = 50, y0 = 50, r = chart_minor_center_circle_radius),
              linewidth = ident_linewidth,
              colour = cols_gpcds$chart_tertiary_darker,
              fill = cols_gpcds$chart_tertiary_darker) +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size * ident_minor_disk_factor,
             show.legend = FALSE) +
  scale_colour_manual(
    values = c(
      "secondary" = cols_gpcds$chart_secondary,
      "tertiary" = cols_gpcds$chart_tertiary
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = cols_gpcds$chart_primary,
      colour = cols_gpcds$chart_primary
    )
  )

ggsave("brand-identity/ident_minor_chart.png",
       gg_chart_ident_minor,
       width = 8,
       height = 8)


# Story -------------------------------------------------------------------

story_x_coords <- list(min = 15, max = 15 *3)
story_y_coords <- 20
story_arrow_y_adjustment <- 7.5
story_arrow_x_adjustment <- 2.5

gg_story_ident_major <- tibble(
  x = c(story_x_coords$min, story_x_coords$max, 100 - story_x_coords$min, 100 - story_x_coords$max),
  y = c(100 - story_y_coords, 100 - story_y_coords, story_y_coords, story_y_coords),
  colour = c("secondary", "tertiary", "secondary", "tertiary")
) %>%
  ggplot() +
  geom_point(aes(x, y, colour = colour),
             size = ident_disk_size,
             show.legend = FALSE) +
  annotate("curve",
           x = story_x_coords$min + story_arrow_x_adjustment,
           y = 100 - story_y_coords + story_arrow_y_adjustment,
           xend = story_x_coords$max - story_arrow_x_adjustment,
           yend = 100 - story_y_coords + story_arrow_y_adjustment,
           linewidth = ident_linewidth - 2,
           colour = cols_gpcds$story_tertiary_darker,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - story_x_coords$min - story_arrow_x_adjustment,
           y = story_y_coords - story_arrow_y_adjustment,
           xend = 100 - story_x_coords$max + story_arrow_x_adjustment,
           yend = story_y_coords - story_arrow_y_adjustment,
           linewidth = ident_linewidth - 2,
           colour = cols_gpcds$story_tertiary_darker,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  geom_text(
    x = 50,
    y = 50,
    label = "DATA\nSTORY",
    size = ident_textsize,
    colour = "white",
    family = "Futura",
    lineheight = 0.9
  ) +
  scale_colour_manual(
    values = c(
      "secondary" = cols_gpcds$story_secondary,
      "tertiary" = cols_gpcds$story_tertiary
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = cols_gpcds$story_primary,
      colour = cols_gpcds$story_primary
    )
  )

ggsave("brand-identity/ident_major_story.png",
       gg_story_ident_major,
       width = 8,
       height = 8)

