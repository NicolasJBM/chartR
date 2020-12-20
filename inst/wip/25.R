library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

edges = flare$edges %>% filter(to %in% from) %>% droplevels()
vertices = flare$vertices %>% filter(name %in% c(edges$from, edges$to)) %>% droplevels()
vertices$size=runif(nrow(vertices))

mygraph <- graph_from_data_frame( edges, vertices=vertices )

chart <- ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = depth)) +
  geom_node_label(aes(label=shortName, filter=leaf, size=size)) +
  scale_label_size(range = c(0.1,0.5)) +
  theme_void() + 
  theme(legend.position="FALSE") +
  scale_fill_viridis()
