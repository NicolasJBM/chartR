library(tibble)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

response <-function(x,y) {-10.4+6.53*x+6.53*y-0.167*x^2-0.167*y^2+0.0500*x*y}
breaks <- function(z)

x<-seq(1,11,.03)
y<-seq(1,11,.03)

chart <- expand.grid(x=x,y=y) %>%
  mutate(z = response(x,y)) %>%
  mutate(b = cut(z,breaks=seq(0,100,len=6))) %>%
  mutate(b = stringr::str_replace_all(b, ",", " - ")) %>%
  mutate(b = stringr::str_remove_all(b, "\\(|\\]")) %>%
  ggplot(aes(x,y)) + 
  geom_tile(aes(fill=b))+
  scale_fill_manual("Response",values=brewer.pal(6,"YlOrRd"))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_fixed()
