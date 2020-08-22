library(dplyr)
library(tibble)
library(ggplot2)
library(ggtern)

chart <- tibble::tibble(
  x = runif(10),
  y = runif(10),
  z = runif(10)
) %>%
  ggtern::ggtern(aes(x=x, y=y, z=z)) + 
  ggtern::stat_density_tern(aes(fill=..level.., alpha=..level..), geom='polygon') +
  ggplot2::scale_fill_gradient2(high = 'blue') +  
  ggplot2::geom_point() +
  ggtern::theme_showarrows() +
  ggplot2::ggtitle('My Favorite Color') +
  ggplot2::xlab('Red') + 
  ggplot2::ylab('Yellow') +
  ggtern::zlab('Blue') +
  ggplot2::guides(color = 'none', fill = 'none', alpha = 'none')
