library("tidyverse")
library("ggforce")
library("tidyverse")
library("GPCDStools")
library("cjhRutils")
library("ggtext")

cols_gpcds <- as.list(deframe(select(colours_gpcds, name, hex_code)))

ident_linewidth <- 5
ident_disk_size <- 25
ident_textsize <- 40
ident_minor_disk_factor <- sqrt(5)
ident_font <- "Futura"

# Solve (x - 50)² + (y - 50)² = 37.5² and y = x
logo_equ_solution <- 50 - 75 / {2 * sqrt(2)}
logo_line_width <- 15
logo_minor_center_circle_radius <- 37.5

gg_logo_experiment_colourful_ring <- tibble(
  x = 100 - logo_equ_solution,
  y = 100 - logo_equ_solution,
  colour = "secondary"
) %>%
  ggplot() +
  geom_arc(
    aes(
      x0 = 0,
      y0 = 0,
      r = 3 * logo_minor_center_circle_radius,
      start = {1/8} * 2 * pi,
      end = {1/8} * 2 * pi + {2 * pi / 3}
    ),
    colour = "black",
    linewidth = logo_line_width + 5
  ) +
  geom_arc(
    aes(
      x0 = 0,
      y0 = 0,
      r = 3 * logo_minor_center_circle_radius,
      start = {1/8} * 2 * pi,
      end = {1/8} * 2 * pi + {2 * pi / 3}
    ),
    colour = cols_gpcds$chart_primary,
    linewidth = logo_line_width
  ) +
  geom_arc(
    aes(
      x0 = 0,
      y0 = 0,
      r = 3 * logo_minor_center_circle_radius,
      start = {1/8} * 2 * pi + {2 * pi / 3},
      end =  {1/8} * 2 * pi + {4 * pi / 3}
    ),
    colour = "black",
    linewidth = logo_line_width + 5
  ) +
  geom_arc(
    aes(
      x0 = 0,
      y0 = 0,
      r = 3 * logo_minor_center_circle_radius,
      start = {1/8} * 2 * pi + {2 * pi / 3},
      end =  {1/8} * 2 * pi + {4 * pi / 3}
    ),
    colour = cols_gpcds$graph_primary,
    linewidth = logo_line_width
  ) +
  geom_arc(
    aes(
      x0 = 0,
      y0 = 0,
      r = 3 * logo_minor_center_circle_radius,
      start = {1/8} * 2 * pi + {4 * pi / 3},
      end =  2 * pi + {1/4} * pi
    ),
    colour = "black",
    linewidth = logo_line_width + 5
  ) +
  geom_arc(
    aes(
      x0 = 0,
      y0 = 0,
      r = 3 * logo_minor_center_circle_radius,
      start = {1/8} * 2 * pi + {4 * pi / 3},
      end =  2 * pi + {1/4} * pi
    ),
    colour = cols_gpcds$plot_primary,
    linewidth = logo_line_width
  ) +
  geom_point(
    aes(x, y),
    fill = cols_gpcds$story_primary,
    colour = "black",
    stroke = 4,
    size = ident_disk_size * ident_minor_disk_factor,
    pch = 21,
    show.legend = FALSE
  ) +
  # geom_point(
  #   aes(x, y),
  #   colour = cols_gpcds$story_tertiary_darker,
  #   size = ident_disk_size * ident_minor_disk_factor * 0.5,
  #   show.legend = FALSE
  # ) +
  geom_richtext(
    aes(
      x = 0,
      y = 0,
      label = str_glue("<span style='color:{cols_gpcds$graph_primary};'>G</span>",
                       "<span style='color:{cols_gpcds$plot_primary};'>P</span>",
                       "<span style='color:{cols_gpcds$chart_primary};'>C</span>",
                       "<span style='color:{cols_gpcds$story_primary};'>DS</span>",
                       .sep = "<span style='color:#000000;'>|</span>")
    ),
    family = "Futura",
    fill = NA,
    label.color = NA,
    lineheight = 1,
    size = ident_textsize * 0.75
  ) +
  coord_fixed() +
  theme_void() +
  NULL

gg_logo_experiment_colourful_ring %>% 
  ggsave(quarto_here("gg_logo_experiment_colourful_ring.png"),
         .,
         width = 8,
         height = 8,
         bg = "white")
