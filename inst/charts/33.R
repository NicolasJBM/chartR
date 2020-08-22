library(ggplot2)
library(ggalt)
library(ggforce)

chart <- ggplot(data = iris,
       aes(x = Sepal.Length, y = Sepal.Width, col=Species, shape=Species, group = Species)) + 
  geom_encircle(data = dplyr::filter(iris, Species == "setosa"),
                colour="red", spread=0.02, fill = "red", alpha = 0.1) +
  geom_encircle(data = dplyr::filter(iris, Species == "versicolor"),
                colour="green", spread=0.02, fill = "green", alpha = 0.1) +
  geom_encircle(data = dplyr::filter(iris, Species == "virginica"),
                colour="blue", spread=0.05, fill = "blue", alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_mark_hull(aes(filter = Species == "versicolor", label = Species)) +
  geom_point() +
  labs(title="Iris Clustering", 
       subtitle="Illustration of a scatterplot",
       caption="Source: Iris") +
  writR::graph_theme()
