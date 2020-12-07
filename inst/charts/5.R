library(tibble)
library(dplyr)
library(ggplot2)
library(chartR)
library(plotly)

chart <- plotly::plot_ly(alpha = 0.6) %>%
  plotly::add_histogram(x = ~rnorm(500)) %>%
  plotly::add_histogram(x = ~rnorm(500) + 1) %>%
  plotly::layout(barmode = "overlay")
