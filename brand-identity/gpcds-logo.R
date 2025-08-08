library("tidyverse")
library("GPCDStools")
library("patchwork")
library("ggtext")
library("ggforce")

cols_gpcds <- as.list(deframe(select(colours_gpcds, name, hex_code)))

x0_small_dot_center <- 500 + (1000 / (1.618 * 2 * sqrt(2)))
r_small_dot <- 1000 / {1.618 * 2 * 3}
linewidth_circles <- 5
linewidth_circles_favicon <- linewidth_circles * 3


degree_change <- 8
func_graph_line  <- function(x) tan({45 + degree_change} * pi / 180) * x + r_small_dot + linewidth_circles * 2

func_story_line  <- function(x) tan({45 - degree_change} * pi / 180) * x - tan({45 - degree_change} * pi / 180) * {r_small_dot + linewidth_circles * 2}

gg_logo_colour <- ggplot() +
  aes(xmin = -1000, xmax = 2000, y = -1000, ymax = 2000) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$other_primary) +
  geom_ribbon(stat = 'function', fun = func_graph_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$graph_primary) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = -Inf, x = 1000),
              fill = cols_gpcds$story_primary) +
  # geom_function(fun = func_story_line,
  #               colour = cols_gpcds$story_tertiary_darker,
  #               linewidth = linewidth_circles) +
  # geom_function(fun = func_graph_line,
  #               colour = cols_gpcds$story_tertiary_darker,
  #               linewidth = linewidth_circles) +
  geom_circle(aes(x0 = 500,
           y0 = 500,
           r = 1000 / {1.618 * 2}),
           fill = cols_gpcds$chart_primary, 
           linewidth = linewidth_circles,
           # linewidth = 0,
           colour = cols_gpcds$story_tertiary_darker) +
  geom_circle(aes(
    x0 = x0_small_dot_center,
    y0 = x0_small_dot_center,
    r = r_small_dot),
    fill = cols_gpcds$plot_primary,
    linewidth = linewidth_circles,
    linewidth = 0,
    colour = cols_gpcds$story_tertiary_darker) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 1000),
                  ylim = c(0, 1000)) +
  theme_void()

gg_logo_colour_favicon <- ggplot() +
  aes(xmin = -1000, xmax = 2000, y = -1000, ymax = 2000) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$other_primary) +
  geom_ribbon(stat = 'function', fun = func_graph_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$graph_primary) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = -Inf, x = 1000),
              fill = cols_gpcds$story_primary) +
  # geom_function(fun = func_story_line,
  #               colour = cols_gpcds$story_tertiary_darker,
  #               linewidth = linewidth_circles) +
  # geom_function(fun = func_graph_line,
  #               colour = cols_gpcds$story_tertiary_darker,
  #               linewidth = linewidth_circles) +
  geom_circle(aes(x0 = 500,
                  y0 = 500,
                  r = 1000 / {1.618 * 2}),
              fill = cols_gpcds$chart_primary, 
              linewidth = linewidth_circles_favicon,
              colour = cols_gpcds$story_tertiary_darker) +
  geom_circle(aes(
    x0 = x0_small_dot_center,
    y0 = x0_small_dot_center,
    r = r_small_dot),
    fill = cols_gpcds$plot_primary,
    linewidth = linewidth_circles_favicon,
    colour = cols_gpcds$story_tertiary_darker) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 1000),
              ylim = c(0, 1000)) +
  theme_void()

gg_logo_white_on_black <- ggplot() +
  aes(xmin = 0, xmax = 10, y = 0, ymax = 10) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = "white") +
  geom_ribbon(stat = 'function', fun = func_graph_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$black_alterative) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = -Inf, x = 1000),
              fill = cols_gpcds$black_alterative) +
  geom_circle(aes(x0 = 500,
                  y0 = 500,
                  r = 1000 / {1.618 * 2}),
              fill = "white", 
              linewidth = linewidth_circles,
              colour = cols_gpcds$black_alterative) +
  geom_circle(aes(
    x0 = x0_small_dot_center,
    y0 = x0_small_dot_center,
    r = r_small_dot),
    fill = cols_gpcds$black_alterative,
    linewidth = linewidth_circles,
    colour = cols_gpcds$black_alterative) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 1000),
              ylim = c(0, 1000)) +
  theme_void()

gg_logo_white_on_black

gg_logo_black_on_white <- ggplot() +
  aes(xmin = 0, xmax = 10, y = 0, ymax = 10) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$black_alterative) +
  geom_ribbon(stat = 'function', fun = func_graph_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = "white") +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = -Inf, x = 1000),
              fill = "white") +
  geom_circle(aes(x0 = 500,
                  y0 = 500,
                  r = 1000 / {1.618 * 2}),
              fill = cols_gpcds$black_alterative, 
              linewidth = linewidth_circles,
              colour = "white") +
  geom_circle(aes(
    x0 = x0_small_dot_center,
    y0 = x0_small_dot_center,
    r = r_small_dot),
    fill = "white",
    linewidth = linewidth_circles,
    colour = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 1000),
              ylim = c(0, 1000)) +
  theme_void()

gg_logo_design_elements <- ggplot() +
  aes(xmin = -1000, xmax = 2000, y = -1000, ymax = 2000) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$other_primary) +
  geom_ribbon(stat = 'function', fun = func_graph_line,
              mapping = aes(ymin = after_stat(y), ymax = Inf, x = 1000),
              fill = cols_gpcds$graph_primary) +
  geom_ribbon(stat = 'function', fun = func_story_line,
              mapping = aes(ymin = after_stat(y), ymax = -Inf, x = 1000),
              fill = cols_gpcds$story_primary) +
  geom_function(fun = function(x) x,
                linewidth = 2,
                linetype = "twodash") +
  # geom_function(fun = func_story_line,
  #               colour = cols_gpcds$story_tertiary_darker,
  #               linewidth = linewidth_circles) +
  # geom_function(fun = func_graph_line,
  #               colour = cols_gpcds$story_tertiary_darker,
  #               linewidth = linewidth_circles) +
  geom_circle(aes(x0 = 500,
                  y0 = 500,
                  r = 1000 / {1.618 * 2}),
              fill = cols_gpcds$chart_primary, 
              linewidth = linewidth_circles,
              # linewidth = 0,
              colour = cols_gpcds$story_tertiary_darker) +
  geom_circle(aes(x0 = 0,
                  y0 = 0,
                  r = r_small_dot),
              fill = cols_gpcds$plot_primary, 
              linewidth = linewidth_circles,
              # linewidth = 0,
              colour = cols_gpcds$story_tertiary_darker) +
  geom_circle(aes(
    x0 = x0_small_dot_center,
    y0 = x0_small_dot_center,
    r = r_small_dot),
    fill = cols_gpcds$plot_primary,
    linewidth = linewidth_circles,
    linewidth = 0,
    colour = cols_gpcds$story_tertiary_darker) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 1000),
              ylim = c(0, 1000)) +
  theme_void()



ggsave(here::here("brand-identity/logo_gpcds_design_elements.png"),
       gg_logo_design_elements,
       width = 10,
       height = 10)



ggsave(here::here("brand-identity/logo_gpcds_colour_favicon.png"),
       gg_logo_colour_favicon,
       width = 10,
       height = 10)

ggsave(here::here("brand-identity/logo_gpcds_colour.png"),
       gg_logo_colour,
       width = 10,
       height = 10)

ggsave(here::here("brand-identity/logo_gpcds_black_on_white.png"),
       gg_logo_black_on_white,
       width = 10,
       height = 10)


ggsave(here::here("brand-identity/logo_gpcds_white_on_black.png"),
       gg_logo_white_on_black,
       width = 10,
       height = 10)

