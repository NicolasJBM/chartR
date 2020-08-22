library(tidyverse)
library(maps)
library(viridis)
library(ggrepel)

FR <- map_data("world") %>% filter(region=="France")
data <- world.cities %>% filter(country.etc=="France")

chart <- data %>%
  arrange(pop) %>% # This reorder your data frame
  mutate( name=factor(name, unique(name))) %>% 
  mutate(pop=pop/1000000) %>%
  ggplot() +
  geom_polygon(data = FR, aes(x=long, y = lat, group = group), fill="grey51", alpha=0.5) +
  geom_point( data=arrange(data, pop), aes(x=long, y=lat, color=pop, size = pop), alpha = 0.5) +
  geom_text_repel( data=arrange(data, pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5, color = "white") +
  theme_void() + ylim(40,52) + xlim(-5,15) +
  coord_map() +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1,12)) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9)) +
  scale_color_viridis(option="magma", trans="log", name="Population (in M)" ) +
  theme(legend.position="none")+ 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in France") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "grey81"),
    plot.background = element_rect(fill = "grey11", color = NA), 
    panel.background = element_rect(fill = "grey11", color = NA), 
    legend.background = element_rect(fill = "grey11", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "white", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )
