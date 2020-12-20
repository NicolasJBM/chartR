library(ggforce)
library(ggplot2)
library(tibble)

base <- tibble::tibble(
  Source = c("A","A","A","B"),
  Destination = c("A","B","B","B"),
  Color = c("A","A","B","B"),
  Count = c(10,20,15,30)
) %>%
  ggforce::gather_set_data(1:2)

chart <- base %>%
  ggplot(aes(x, id = id, split = y, value = Count)) +
  geom_parallel_sets(aes(fill = Color), alpha = 0.75, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white', angle = 0, size = 2.5) +
  scale_fill_grey() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  coord_flip()

