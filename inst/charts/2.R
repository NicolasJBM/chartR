library(tibble)
library(dplyr)
library(ggplot2)
library(writR)

chart <- dplyr::bind_rows(list(
  data.frame( x=rnorm(500, 10, 1.9), y=rnorm(500, 10, 1.2) ),
  data.frame( x=rnorm(500, 14.5, 1.9), y=rnorm(500, 14.5, 1.9) ),
  data.frame( x=rnorm(500, 9.5, 1.9), y=rnorm(500, 15.5, 1.9) )
)) %>%
  ggplot(aes(
    x = x,
    y = y
  )) +
  ggplot2::stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  ggplot2::stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  #ggplot2::geom_hex(bins = 70) +
  ggplot2::scale_fill_distiller(palette= "Spectral", direction = -1) +
  ggplot2::theme_minimal()
