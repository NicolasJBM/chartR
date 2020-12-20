library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)


sentence <- udpipe("This chart shows the grammatical relationships between words in a sentence", "english")

sentence <- sentence[!is.na(sentence$head_token_id), ]
sentence <- sentence[x$sentence_id %in% min(sentence$sentence_id), ]

edges <- sentence[sentence$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
edges$label <- edges$dep_rel

chart <- graph_from_data_frame(edges,
                               vertices = sentence[, c("token_id", "token", "lemma", "upos", "xpos", "feats")],
                               directed = TRUE)

chart <- ggraph(chart, layout = "linear") +
  geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                arrow = grid::arrow(length = unit(4, 'mm'), ends = "last", type = "closed"),
                end_cap = ggraph::label_rect("wordswordswords"),
                label_colour = "red", check_overlap = TRUE, label_size = 2) +
  geom_node_label(ggplot2::aes(label = token), col = "darkgreen", size = 2, fontface = "bold") +
  geom_node_text(ggplot2::aes(label = upos), nudge_y = -0.35, size = 2) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Word connections", subtitle = "Illustration of the method")


