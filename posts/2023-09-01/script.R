
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)

library(tidygraph)
library(ggraph)
library(ggupset)


# Load data ---------------------------------------------------------------

fair_use_cases <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv')
fair_use_findings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_findings.csv')



# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

case_categories <- fair_use_cases %>% 
  separate_longer_delim(categories,
                        delim = ";") %>% 
  mutate(categories = str_trim(categories),
         categories = str_to_sentence(categories)) %>% 
  select(case, categories) %>% 
  mutate(n_category = n(),
         .by = categories) %>% 
  filter(n_category >= 10)


nodes_categories <- case_categories %>% 
  distinct(categories) %>% 
  arrange(categories) %>% 
  mutate(id = row_number()) %>% 
  select(id, name = categories)


categories_to_cases <- case_categories %>% 
  left_join(nodes_categories,
            by = c("categories" = "name")) %>% 
  select(case, id)


edges_categories <- categories_to_cases %>% 
  inner_join(categories_to_cases, by = "case") %>%
  filter(id.x != id.y) %>% 
  select(from = id.x,
         to = id.y,
         case) %>% 
  summarise(cases = list(case),
            .by = c(from, to)) %>% 
  rowwise() %>% 
  mutate(n_cases = length(cases)) %>% 
  ungroup()


tbl_graph(nodes_categories, edges_categories) %>% 
  ggraph(layout = 'linear', circular = TRUE) +
  geom_node_point() +
  geom_edge_arc(aes(width = n_cases)) +
  geom_node_label(aes(label = name))



tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE)

case_categories %>% 
  summarise(categories = list(categories),
            .by = case) %>% 
  ggplot(aes(x=categories)) +
  # geom_bar() +
  scale_x_upset(n_intersections = 10)




# Looking at findings -----------------------------------------------------

fair_use_findings

# Plot --------------------------------------------------------------------




# Graph -------------------------------------------------------------------


# Chart -------------------------------------------------------------------


# Data story --------------------------------------------------------------






# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path(yr, date_chr, "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "roboto"
)
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", social
)


# Plot --------------------------------------------------------------------



# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path(yr, date_chr, paste0(date_strip, ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
