library(dplyr)
library(collapsibleTree)

chart <- tibble::tibble(
  continent = c('Europe','Europe','Europe','Asia','Asia','America','America','America'),
  country = c('France','France','Spain','Japan','Japan','Canada','Canada','Mexico'),
  city = c('Paris','Lyon','Madrid','Tokyo','Kyoto','Montreal','Vancouver','Monterrey')
) %>%
  collapsibleTree::collapsibleTree(
    hierarchy = c('continent', 'country', 'city'),
    width = 800,
    zoomable = FALSE
  )
