library(dplyr)
library(maps)
library(ggplot2)
library(treemap)

data("GNI2014")

basemap = map_data("world") %>%
  mutate(iso3 = countrycode::countrycode(region, origin = "country.name", destination = "iso3c", warn = FALSE)) %>%
  left_join(GNI2014, by = "iso3") %>%
  mutate(population = log(population))

chart <- ggplot(basemap, aes(long, lat, group=group, fill=population)) + 
  geom_polygon(show.legend = F) +
  scale_fill_viridis_c(option = "magma") +
  ggthemes::theme_map()
