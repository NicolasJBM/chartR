library(ggplot2)
library(ggridges)

chart <- ggplot2::ggplot(diamonds, aes(x = price, y = cut)) +
  ggridges::geom_density_ridges(scale = 4) +
  ggridges::theme_ridges() +
  ggplot2::scale_y_discrete(expand = c(0.01, 0)) +
  ggplot2::scale_x_continuous(expand = c(0, 0))
