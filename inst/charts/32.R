library(dplyr)
library(maps)
library(ggplot2)
library(maptools)
library(treemap)
library(cartogram)
library(broom)
library(tibble)

data("GNI2014")
data(wrld_simpl)

pop <- GNI2014 %>%
  select(iso3, population) %>%
  filter(iso3 %in% c("AUT", "BEL", "DNK", "FRA", "DEU", "HUN", "IRL", "ITA", "LUX", "NLD", "PRT", "ESP", "GBR")) %>%
  remove_rownames() %>%
  column_to_rownames("iso3")
europe = wrld_simpl[wrld_simpl$ISO3 %in% rownames(pop),]
europe$POPULATION <- pop[as.character(europe$ISO3), "population"]
cartogram <- cartogram_cont(europe, "POPULATION", itermax=3, maxSizeError = 1.1)
spdf_fortified <- tidy(cartogram)
spdf_fortified = spdf_fortified %>% left_join(. , cartogram@data, by=c("id"="ISO3")) 


library(viridis)
chart <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = POPULATION/1000000, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  scale_fill_viridis(name="Population (M)", breaks=c(1,25,50,75,100),
                     guide = guide_legend(keyheight = unit(5, units = "mm"), keywidth=unit(12, units = "mm"),
                                          label.position = "top", title.position = 'top', nrow=1)) +
  ylim(30,60) + xlim(-15,25) +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  coord_map()


