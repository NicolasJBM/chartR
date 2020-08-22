library(lubridate)
library(dplyr)
library(ggplot2)

day=as.Date('2017-06-14') - 1:90
value=runif(90) + seq(-44, 45)^2 / 10000
data=data.frame(day, value)

don <- data %>% mutate(week = as.Date(cut(day, breaks = 'week'))) %>%
  group_by(week) %>% 
  summarise(average = mean(value)) 

chart <- ggplot(don, aes(x=week, y=average)) +
  geom_line() + 
  geom_point() +
  geom_area(fill=alpha('slateblue',0.2)) +
  scale_x_date(date_labels = '%W-%b', date_breaks='1 week') +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  writR::graph_theme()
