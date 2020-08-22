library(ggplot2)
library(dplyr)

data("diamonds")

chart <- diamonds %>%
  group_by(cut, clarity, color) %>%
  summarize(price = mean(price)) %>%
  ggplot(aes(x = cut, y = price, group = color, fill = color)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(clarity~.) +
  coord_flip() +
  writR::graph_theme(legend.position = "right")

