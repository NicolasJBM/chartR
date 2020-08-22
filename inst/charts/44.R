library(dplyr)
library(ggplot2)
library(plotly)
library(gapminder)

chart <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = continent, label = country, frame = year)
) +
  #geom_point(show.legend = FALSE, alpha = 0.7) +
  geom_text() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy") +
  writR::graph_theme(legend.position = "none")

chart <- ggplotly(chart, height = 600, width = 800) %>%
  animation_opts(frame = 1000,
                 easing = "linear",
                 redraw = FALSE) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(size = 20, color="blue"))
  )
