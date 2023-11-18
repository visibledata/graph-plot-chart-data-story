library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggalluvial)

naniar::vis_miss(msleep)

many_genus_animals <- msleep %>% 
  select(name:order) %>% 
  drop_na(vore) %>% 
  filter(! genus == name) %>% 
  add_count(order, name = "n_order") %>% 
  add_count(genus, name = "n_genus") %>% 
  # filter(n_genus > 1) %>% 
  mutate(kingdom = "animal")

ggplot(data = many_genus_animals,
       aes(axis1 = kingdom, axis2 = order, axis3 = genus,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) 


msleep %>% 
  select(name:conservation) %>% 
  drop_na(vore, conservation) %>% 
  filter(! genus == name) %>% 
  add_count(order, conservation, name = "n_order_conservation") %>% 
  add_count(order, name = "n_order") %>% 
  mutate(order = ifelse(n_order > 2, order, "Other")) %>% 
  mutate(order = fct_reorder(order, n_order),
         order = fct_rev(order)
         ) %>%
  ggplot() +
  aes(axis1 = order, axis2 = conservation,
      y = n_order_conservation) +
  geom_alluvium(aes(fill = vore)) +
  geom_stratum(absolute = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
            min.y = 5,
            absolu)


# GGRAPH FAILURES ---------------------------------------------------------



edges_many_kingdoms <- select(many_genus_animals, kingdom, order, vore) %>% 
  distinct() %>% 
  select(from = kingdom,
         to = order) %>% 
  mutate(diet = "undefined")

edges_many_orders <- select(many_genus_animals, genus, order, vore) %>% 
  distinct() %>% 
  select(from = order,
         to = genus,
         diet = vore)

edges_many_animals <- select(many_genus_animals, name, genus, vore) %>% 
  distinct() %>% 
  select(from = genus,
         to = name,
         diet = vore)

edges_many_kingdoms %>% 
  bind_rows(edges_many_orders) %>% 
  bind_rows(edges_many_animals) %>% 
  # filter(diet == "omni") %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  ggraph(layout = "dendrogram",
         circular = TRUE) +
  geom_node_point() +
  # geom_node_label(aes(label = name)) +
  geom_edge_elbow(aes(colour = diet))


# Just order --------------------------------------------------------------

many_genus_animals <- msleep %>% 
  select(name:order) %>% 
  drop_na(vore) %>% 
  filter(! genus == name) %>% 
  add_count(order, name = "n_order") %>% 
  add_count(genus, name = "n_genus") %>% 
  # filter(n_genus > 1) %>% 
  mutate(kingdom = "animal")

edges_many_genus <- select(many_genus_animals, kingdom, genus, vore) %>% 
  distinct() %>% 
  select(from = kingdom,
         to = genus,
         diet = vore)

edges_many_animals <- select(many_genus_animals, name, genus, vore) %>% 
  distinct() %>% 
  select(from = genus,
         to = name,
         diet = vore) 

edges_many_genus %>% 
  bind_rows(edges_many_animals) %>% 
  # filter(diet == "omni") %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  ggraph(layout = "dendrogram", circular = TRUE) +
  geom_node_point() +
  geom_node_label(aes(label = name)) +
  geom_edge_link(aes(colour = diet))



edges_animal <- msleep_animals %>% 
  left_join(msleep_animals,
            by = c("vore" = "vore")) %>% 
  select(from = name.x, 
         to = name.y)


nodes_animals <- msleep_animals %>% 
  rename("id" = "name")

graph_animal_order <- tbl_graph(nodes_animals, edges_animal)


graph_animal_order %>% 
  ggraph() +
  geom_node_point() +
  geom_edge_link()



# FOO ---------------------------------------------------------------------

library(ggraph)
library(igraph)
library(tidyverse)

# data: edge list
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
edges <- rbind(d1, d2)

# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices)

example_g <- mygraph %>% as_tbl_graph()

example_g

example_g %>% 
  ggraph(layout = "dendrogram") +
  geom_edge_diagonal() 

