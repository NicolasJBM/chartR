library(tibble)
library(dplyr)
library(ggplot2)
library(writR)

chart <- dplyr::bind_rows(
  dplyr::bind_rows(list(
    tibble::tibble( color = "blue", price=rnorm(500, 10, 1.9) ),
    tibble::tibble( color = "green", price=rnorm(500, 14.5, 1.9) ),
    tibble::tibble( color = "red", price=rnorm(500, 9.5, 1.9) )
  )) 
) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = price,
    fill = color
  )) +
  ggplot2::geom_density(alpha = 0.2, position = "stack") +
  ggplot2::scale_fill_manual(values = c("blue","green","red")) +
  writR::graph_theme()
