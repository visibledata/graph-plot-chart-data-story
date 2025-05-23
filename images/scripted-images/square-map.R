library("tidyverse")
library("sf")
library("rnaturalearthdata")
library("GPCDStools")
sf_use_s2(FALSE)

cols_gpcds <- colours_gpcds |> 
  select(name, hex_code) |> 
  deframe() |> 
  as.list()

continent_sf <- countries110 |> 
  filter(name_en != "Antarctica") |> 
  select(continent) |> 
  group_by(continent) |> 
  summarise(foo = 1)


orig_bbox <- continent_sf |> 
  st_bbox() |> 
  as.list()

lat_range <- orig_bbox$ymax - orig_bbox$ymin

long_orig <- -20

continent_sf |> 
  filter(continent %in% c("Africa", "Asia", "Europe", "North America", "South America")) |> 
  ggplot() +
  geom_sf(aes(fill = continent), show.legend = FALSE, colour = "white", linewidth = 1) +
  scale_fill_manual(values = c(cols_gpcds$graph_primary, cols_gpcds$plot_primary, cols_gpcds$chart_primary, cols_gpcds$story_primary, cols_gpcds$other_primary)) +
  coord_sf(xlim = c(long_orig - lat_range / 2, long_orig + lat_range / 2),
  ylim = c(orig_bbox$ymin, orig_bbox$ymax)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F8F6EF"))

