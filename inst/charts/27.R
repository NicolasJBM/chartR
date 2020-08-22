library(treemap)
library(treemapify)
library(ggplot2)
library(dplyr)

data('GNI2014')

chart <- ggplot(GNI2014, aes(area = population, fill = continent, label = country,
                    subgroup = continent)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = 'centre', grow = F, alpha = .25, colour =
                               'white', fontface = 'italic', min.size = 0) +
  geom_treemap_text(colour = 'black', place = 'topleft', reflow = T) +
  writR::graph_theme(legend.position = 'none')
