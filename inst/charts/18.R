
library(ggplot2)
library(ggmosaic)

chart <- ggplot(data = fly) +
  geom_mosaic(aes(x = product(FlightFreq, Region), fill=FlightFreq), na.rm=TRUE, offset = 0.02) +
  labs(x='Region', y=' ') +
  coord_flip() +
  writR::graph_theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 60))
