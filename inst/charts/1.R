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
  ggplot2::ggplot(aes(
    x = color,
    y = price,
    fill = color
  )) +
  ggplot2::geom_boxplot(alpha = 0.6) +
  ggplot2::scale_fill_manual(values = c("blue","green","red")) +
  ggplot2::geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, binwidth = 0.25,
               fill="grey", alpha = 0.5) +
  writR::graph_theme()
