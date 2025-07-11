library("tidyverse")
library("GPCDStools")
library("rsvg")

cols_gpcds <- as.list(deframe(select(colours_gpcds, name, hex_code)))

rlogo_original_chr <- readLines("brand-identity/r-logo_original.svg")


rlogo_original_chr %>% 
charToRaw() %>% 
  rsvg_png(file = "brand-identity/r-logo_original.png")

col_rlogo_ring_light <- "#cbced0"
col_rlogo_ring_dark <- "#84838b"

col_rlogo_rletter_light <- "#276dc3"
col_rlogo_rletter_dark <- "#165caa"

make_new_r_logo <- function(ring_light, ring_dark, letter_light, letter_dark){
  
  original_colours <- c(col_rlogo_ring_light, col_rlogo_ring_dark, col_rlogo_rletter_light, col_rlogo_rletter_dark)
  
  vec_replacements <- setNames(c(ring_light, ring_dark, letter_light, letter_dark), original_colours)
  
  str_replace_all(rlogo_original_chr, vec_replacements)
  
}

make_new_r_logo(cols_gpcds$graph_primary, cols_gpcds$graph_tertiary_darker, cols_gpcds$graph_primary, cols_gpcds$graph_secondary) %>% 
  writeLines("brand-identity/r-logo_graph_colours.svg")

make_new_r_logo(cols_gpcds$graph_primary, cols_gpcds$graph_tertiary_darker, cols_gpcds$graph_primary, cols_gpcds$graph_secondary) %>% 
  charToRaw() %>% 
  rsvg_png(file = "brand-identity/r-logo_graph_colours.png")


make_new_r_logo(cols_gpcds$plot_primary, cols_gpcds$plot_tertiary_darker, cols_gpcds$plot_primary, cols_gpcds$plot_secondary) %>% 
  writeLines("brand-identity/r-logo_plot_colours.svg")

make_new_r_logo(cols_gpcds$plot_primary, cols_gpcds$plot_tertiary_darker, cols_gpcds$plot_primary, cols_gpcds$plot_secondary) %>% 
  charToRaw() %>% 
  rsvg_png(file = "brand-identity/r-logo_plot_colours.png")


make_new_r_logo(cols_gpcds$chart_primary, cols_gpcds$chart_tertiary_darker, cols_gpcds$chart_primary, cols_gpcds$chart_secondary) %>% 
  writeLines("brand-identity/r-logo_chart_colours.svg")

make_new_r_logo(cols_gpcds$chart_primary, cols_gpcds$chart_tertiary_darker, cols_gpcds$chart_primary, cols_gpcds$chart_secondary) %>% 
  charToRaw() %>% 
  rsvg_png(file = "brand-identity/r-logo_chart_colours.png")


make_new_r_logo(cols_gpcds$story_primary, cols_gpcds$story_tertiary_darker, cols_gpcds$story_primary, cols_gpcds$story_secondary) %>% 
  writeLines("brand-identity/r-logo_story_colours.svg")

make_new_r_logo(cols_gpcds$story_primary, cols_gpcds$story_tertiary_darker, cols_gpcds$story_primary, cols_gpcds$story_secondary) %>% 
  charToRaw() %>% 
  rsvg_png(file = "brand-identity/r-logo_story_colours.png")





foo <- rsvg::rsvg("brand-identity/r-logo_graph_colours.svg")

rsvg_png(charToRaw(foo), file = "brand-identity/r-logo_graph_colours.png")



options(example.ask=FALSE)
tmp <- tempfile()
svglite::svglite(tmp, width = 10, height = 7)
ggplot2::qplot(mpg, wt, data = mtcars, colour = factor(cyl))
dev.off()

# convert directly into a vector or bitmap graphics format
rsvg_pdf(tmp, "out.pdf")
