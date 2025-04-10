library("tidyverse")
library("ggforce")
library("tidyverse")
library("GPCDStools")
library("cjhRutils")

cols_gpcds <- as.list(deframe(select(colours_gpcds, name, hex_code)))

ident_linewidth <- 5
ident_disk_size <- 25
ident_textsize <- 40
ident_minor_disk_factor <- sqrt(5)
ident_font <- "Futura"

# Solve (x - 50)² + (y - 50)² = 37.5² and y = x
chart_equ_solution <- 50 - 75 / {2 * sqrt(2)}
chart_minor_center_circle_radius <- 37.5

logomarkfill_colour_geoms_colourful <- "white"
logomarkstroke_colour_geoms_colourful <- "#4F4F4F"
logomarkstroke_size <- 5
logomarkstroke_segment_adjustment <- 8
logomarkstroke_curve_adjustment <- 2

logomarkstory_arrow_x_adjustment <- 2.5
logomarkstory_arrow_y_adjustment <- 15
logomarkstory_x_coords <- list(min = 25, max = 75)


# LOGOMARKS ---------------------------------------------------------------

## ==== Colour mark

gg_logomark_colour <- ggplot() +
  ## ==== graph
  annotate("rect",
           xmin = -100, xmax = 0, ymin = 0, ymax = 100,
           fill = cols_gpcds$graph_primary,
           colour = cols_gpcds$graph_primary) +
  annotate(
    "segment",
    x = -20, 
    xend = -80,
    y = 50, 
    yend = 50,
    linewidth = ident_linewidth + logomarkstroke_segment_adjustment,
    colour = logomarkstroke_colour_geoms_colourful) +
  annotate("point",
           x = c(-20, -80),
           y = c(50, 50),
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkfill_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  annotate(
    "segment",
    x = -20, 
    xend = -80,
    y = 50, 
    yend = 50,
    linewidth = ident_linewidth + 2,
    colour = logomarkfill_colour_geoms_colourful) +
  ## ==== plot
  annotate("rect",
           xmin = 0, xmax = 100, ymin = 0, ymax = 100,
           fill = cols_gpcds$plot_primary,
           colour = cols_gpcds$plot_primary) +
  annotate("point",
           x = c(30, 70),
           y = c(70, 30),
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkfill_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  ## ==== chart
  annotate("rect",
           xmin = -100, xmax = 0, ymin = -100, ymax = 0,
           fill = cols_gpcds$chart_primary,
           colour = cols_gpcds$chart_primary) +
  geom_circle(aes(x0 = -50, y0 = -50, r = chart_minor_center_circle_radius),
              linewidth = logomarkstroke_size - 1,
              colour = logomarkstroke_colour_geoms_colourful,
              fill = logomarkfill_colour_geoms_colourful) +
  annotate("point",
           x = -chart_equ_solution,
           y = -chart_equ_solution,
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkfill_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  ## ==== story
  annotate("rect",
           xmin = 0, xmax = 100, ymin = 0, ymax = -100,
           fill = cols_gpcds$story_primary,
           colour = cols_gpcds$story_primary) +
  annotate("point",
           x = c(25, 75),
           y = -50,
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkfill_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  annotate("curve",
           x = logomarkstory_x_coords$min + logomarkstory_arrow_x_adjustment,
           y = -50 + logomarkstory_arrow_y_adjustment,
           xend = logomarkstory_x_coords$max - logomarkstory_arrow_x_adjustment,
           yend = -50 + logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth + logomarkstroke_curve_adjustment,
           colour = logomarkstroke_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = logomarkstory_x_coords$min + logomarkstory_arrow_x_adjustment,
           y = -50 + logomarkstory_arrow_y_adjustment,
           xend = logomarkstory_x_coords$max - logomarkstory_arrow_x_adjustment,
           yend = -50 + logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth - logomarkstroke_curve_adjustment,
           colour = logomarkfill_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - logomarkstory_x_coords$min - logomarkstory_arrow_x_adjustment,
           y = -50 - logomarkstory_arrow_y_adjustment,
           xend = 100 - logomarkstory_x_coords$max + logomarkstory_arrow_x_adjustment,
           yend = -50 - logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth + logomarkstroke_curve_adjustment,
           colour = logomarkstroke_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - logomarkstory_x_coords$min - logomarkstory_arrow_x_adjustment,
           y = -50 - logomarkstory_arrow_y_adjustment,
           xend = 100 - logomarkstory_x_coords$max + logomarkstory_arrow_x_adjustment,
           yend = -50 - logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth - logomarkstroke_curve_adjustment,
           colour = logomarkfill_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  ## ==== seperators 
  geom_segment(aes(y = -100, yend = 100, x = 0, xend = 0),
               colour = logomarkstroke_colour_geoms_colourful,
               linewidth = logomarkstroke_size - 2) +
  geom_segment(aes(x = -100, xend = 100, y = 0, yend = 0),
               colour = logomarkstroke_colour_geoms_colourful,
               linewidth = logomarkstroke_size - 2) +
  annotate("rect",
           xmin = -100, xmax = 100, ymin = -100, ymax = 100,
           fill = NA,
           colour = logomarkstroke_colour_geoms_colourful,
           linewidth = logomarkstroke_size) +
  ## ==== scales
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(-100, 100), ylim = c(-100, 100)) +
  theme_void()

gg_logomark_colour %>% 
  ggsave("brand-identity/logomark_colour.png",
         .,
         width = 10,
         height = 10)

## ==== Black on white

gg_logomark_black_on_white <- ggplot() +
  ## ==== graph
  annotate("rect",
           xmin = -100, xmax = 0, ymin = 0, ymax = 100,
           fill = "white",
           colour = cols_gpcds$graph_primary) +
  annotate(
    "segment",
    x = -20, 
    xend = -80,
    y = 50, 
    yend = 50,
    linewidth = ident_linewidth + logomarkstroke_segment_adjustment,
    colour = logomarkstroke_colour_geoms_colourful) +
  annotate("point",
           x = c(-20, -80),
           y = c(50, 50),
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkstroke_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  annotate(
    "segment",
    x = -20, 
    xend = -80,
    y = 50, 
    yend = 50,
    linewidth = ident_linewidth + 2,
    colour = logomarkstroke_colour_geoms_colourful) +
  ## ==== plot
  annotate("rect",
           xmin = 0, xmax = 100, ymin = 0, ymax = 100,
           fill = "white",
           colour = cols_gpcds$plot_primary) +
  annotate("point",
           x = c(30, 70),
           y = c(70, 30),
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkstroke_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  ## ==== chart
  annotate("rect",
           xmin = -100, xmax = 0, ymin = -100, ymax = 0,
           fill = "white",
           colour = cols_gpcds$chart_primary) +
  geom_circle(aes(x0 = -50, y0 = -50, r = chart_minor_center_circle_radius),
              linewidth = logomarkstroke_size - 1,
              colour = logomarkstroke_colour_geoms_colourful,
              fill = logomarkstroke_colour_geoms_colourful) +
  annotate("point",
           x = -chart_equ_solution,
           y = -chart_equ_solution,
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkfill_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  ## ==== story
  annotate("rect",
           xmin = 0, xmax = 100, ymin = 0, ymax = -100,
           fill = "white",
           colour = cols_gpcds$story_primary) +
  annotate("point",
           x = c(25, 75),
           y = -50,
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = logomarkstroke_colour_geoms_colourful,
           stroke = logomarkstroke_size,
           pch = 21) +
  annotate("curve",
           x = logomarkstory_x_coords$min + logomarkstory_arrow_x_adjustment,
           y = -50 + logomarkstory_arrow_y_adjustment,
           xend = logomarkstory_x_coords$max - logomarkstory_arrow_x_adjustment,
           yend = -50 + logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth + logomarkstroke_curve_adjustment,
           colour = logomarkstroke_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = logomarkstory_x_coords$min + logomarkstory_arrow_x_adjustment,
           y = -50 + logomarkstory_arrow_y_adjustment,
           xend = logomarkstory_x_coords$max - logomarkstory_arrow_x_adjustment,
           yend = -50 + logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth - logomarkstroke_curve_adjustment,
           colour = logomarkstroke_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - logomarkstory_x_coords$min - logomarkstory_arrow_x_adjustment,
           y = -50 - logomarkstory_arrow_y_adjustment,
           xend = 100 - logomarkstory_x_coords$max + logomarkstory_arrow_x_adjustment,
           yend = -50 - logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth + logomarkstroke_curve_adjustment,
           colour = logomarkstroke_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - logomarkstory_x_coords$min - logomarkstory_arrow_x_adjustment,
           y = -50 - logomarkstory_arrow_y_adjustment,
           xend = 100 - logomarkstory_x_coords$max + logomarkstory_arrow_x_adjustment,
           yend = -50 - logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth - logomarkstroke_curve_adjustment,
           colour = logomarkstroke_colour_geoms_colourful,
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  ## ==== seperators 
  geom_segment(aes(y = -100, yend = 100, x = 0, xend = 0),
               colour = logomarkstroke_colour_geoms_colourful,
               linewidth = logomarkstroke_size - 2) +
  geom_segment(aes(x = -100, xend = 100, y = 0, yend = 0),
               colour = logomarkstroke_colour_geoms_colourful,
               linewidth = logomarkstroke_size - 2) +
  annotate("rect",
           xmin = -100, xmax = 100, ymin = -100, ymax = 100,
           fill = NA,
           colour = logomarkstroke_colour_geoms_colourful,
           linewidth = logomarkstroke_size) +
  ## ==== scales
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(-100, 100), ylim = c(-100, 100)) +
  theme_void()

gg_logomark_black_on_white %>% 
  ggsave("brand-identity/logomark_black_on_white.png",
         .,
         width = 10,
         height = 10)

## ==== White on black

gg_logomark_white_on_black <- ggplot() +
  ## ==== graph
  annotate("rect",
           xmin = -100, xmax = 0, ymin = 0, ymax = 100,
           fill = logomarkstroke_colour_geoms_colourful,
           colour = cols_gpcds$graph_primary) +
  annotate(
    "segment",
    x = -20, 
    xend = -80,
    y = 50, 
    yend = 50,
    linewidth = ident_linewidth + logomarkstroke_segment_adjustment,
    colour = "white") +
  annotate("point",
           x = c(-20, -80),
           y = c(50, 50),
           size = ident_disk_size,
           colour = "white",
           fill = "white",
           stroke = logomarkstroke_size,
           pch = 21) +
  annotate(
    "segment",
    x = -20, 
    xend = -80,
    y = 50, 
    yend = 50,
    linewidth = ident_linewidth + 2,
    colour = "white") +
  ## ==== plot
  annotate("rect",
           xmin = 0, xmax = 100, ymin = 0, ymax = 100,
           fill = logomarkstroke_colour_geoms_colourful,
           colour = cols_gpcds$plot_primary) +
  annotate("point",
           x = c(30, 70),
           y = c(70, 30),
           size = ident_disk_size,
           colour = "white",
           fill = "white",
           stroke = logomarkstroke_size,
           pch = 21) +
  ## ==== chart
  annotate("rect",
           xmin = -100, xmax = 0, ymin = -100, ymax = 0,
           fill = logomarkstroke_colour_geoms_colourful,
           colour = cols_gpcds$chart_primary) +
  geom_circle(aes(x0 = -50, y0 = -50, r = chart_minor_center_circle_radius),
              linewidth = logomarkstroke_size - 1,
              colour = "white",
              fill = "white") +
  annotate("point",
           x = -chart_equ_solution,
           y = -chart_equ_solution,
           size = ident_disk_size,
           colour = logomarkstroke_colour_geoms_colourful,
           fill = "white",
           stroke = logomarkstroke_size,
           pch = 21) +
  ## ==== story
  annotate("rect",
           xmin = 0, xmax = 100, ymin = 0, ymax = -100,
           fill = logomarkstroke_colour_geoms_colourful,
           colour = cols_gpcds$story_primary) +
  annotate("point",
           x = c(25, 75),
           y = -50,
           size = ident_disk_size,
           colour = "white",
           fill = "white",
           stroke = logomarkstroke_size,
           pch = 21) +
  annotate("curve",
           x = logomarkstory_x_coords$min + logomarkstory_arrow_x_adjustment,
           y = -50 + logomarkstory_arrow_y_adjustment,
           xend = logomarkstory_x_coords$max - logomarkstory_arrow_x_adjustment,
           yend = -50 + logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth + logomarkstroke_curve_adjustment,
           colour = "white",
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = logomarkstory_x_coords$min + logomarkstory_arrow_x_adjustment,
           y = -50 + logomarkstory_arrow_y_adjustment,
           xend = logomarkstory_x_coords$max - logomarkstory_arrow_x_adjustment,
           yend = -50 + logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth - logomarkstroke_curve_adjustment,
           colour = "white",
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - logomarkstory_x_coords$min - logomarkstory_arrow_x_adjustment,
           y = -50 - logomarkstory_arrow_y_adjustment,
           xend = 100 - logomarkstory_x_coords$max + logomarkstory_arrow_x_adjustment,
           yend = -50 - logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth + logomarkstroke_curve_adjustment,
           colour = "white",
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  annotate("curve",
           x = 100 - logomarkstory_x_coords$min - logomarkstory_arrow_x_adjustment,
           y = -50 - logomarkstory_arrow_y_adjustment,
           xend = 100 - logomarkstory_x_coords$max + logomarkstory_arrow_x_adjustment,
           yend = -50 - logomarkstory_arrow_y_adjustment,
           linewidth = ident_linewidth - logomarkstroke_curve_adjustment,
           colour = "white",
           arrow = arrow(length = unit(0.6, "cm"), type = "closed"),
           curvature = -0.5,
           ncp = 1000) +
  ## ==== seperators 
  geom_segment(aes(y = -100, yend = 100, x = 0, xend = 0),
               colour = "white",
               linewidth = logomarkstroke_size - 2) +
  geom_segment(aes(x = -100, xend = 100, y = 0, yend = 0),
               colour = "white",
               linewidth = logomarkstroke_size - 2) +
  annotate("rect",
           xmin = -100, xmax = 100, ymin = -100, ymax = 100,
           fill = NA,
           colour = "white",
           linewidth = logomarkstroke_size) +
  ## ==== scales
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(-100, 100), ylim = c(-100, 100)) +
  theme_void()

gg_logomark_white_on_black %>% 
  ggsave("brand-identity/logomark_white_on_black.png",
         .,
         width = 10,
         height = 10)