library(dplyr)
library(tidyr)
library(ggiraphExtra)
library(ggplot2)

chart <- tibble(
  profile = c("A","B","C"),
  dim1 = c(1,2,4),
  dim2 = c(2,2,5),
  dim3 = c(3,2,3),
  dim4 = c(4,3,1),
  dim5 = c(5,3,1)
) %>%
  gather(dimension, value, -profile) %>%
  ggplot(aes(x  = dimension, y = value) ) + 
  geom_polygon(aes(group = profile, color = profile, fill = profile), 
               show.legend = FALSE,
               size = 1.1, alpha = 0.1) +
  geom_line(   aes(group = profile, color = profile),
               size = 1.1, alpha = 0.3) +
  geom_point(aes(x  = dimension, y = value, color = profile), size = 3) +
  guides(color = guide_legend(ncol = 1))+
  theme_minimal() +
  coord_radar()   +
  xlab("") + ylab("") +
  annotate("text", x = 0.5, y = c(0,1,2,3,4,5), label = c("","1","2","3","4","5")) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x  = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
