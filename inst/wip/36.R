library(tidygraph)
library(igraph)
library(ggraph)


network <- play_islands(5, 10, 0.7, 2) %>% 
  activate("nodes") %>%
  mutate(
    name = c(1:50),
    betweenness = centrality_betweenness(),
    community = as.factor(group_infomap())
  ) %>%
  activate("edges") %>%
  mutate(
    betweenness = centrality_edge_betweenness()
  )

chart <- network %>%
  ggraph(layout = 'kk') + 
  geom_edge_arc(
    aes(colour = "grey", width = 1),
    arrow = arrow(length = unit(4, 'mm')),
    end_cap = circle(3, 'mm'),
    curvature = 0.1
  ) + 
  scale_edge_width(range = c(0.5, 0.5), guide = FALSE) +
  scale_edge_color_manual(guide = FALSE, values = c("grey51","grey31")) +
  geom_node_point(aes(size = betweenness, colour = community)) +
  scale_size(range = c(1,10), guide = FALSE) +
  geom_node_text(aes(label = name), color = "white") +
  scale_color_manual(guide = FALSE, values = c("blue","darkgreen","red","purple","orange")) +
  theme_graph()

# With encircled communities
#network %>%
#  cluster_edge_betweenness() %>%
#  plot(network)

