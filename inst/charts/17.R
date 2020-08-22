library(ggplot2)

base <- tibble::tibble(
  Age = as.factor(c(10,20,30,40,50,60,10,20,30,40,50,60)),
  Gender = c('F','F','F','F','F','F','M','M','M','M','M','M'),
  Population = c(100,200,300,400,200,100,50,200,350,250,200,150)
)

chart <- ggplot(data=base) + 
  geom_bar(data=subset(base,Gender=='F'), stat = 'identity', aes(x=Age, y = Population,fill=Gender)) + 
  geom_bar(data=subset(base,Gender=='M'), stat = 'identity', aes(x=Age, y = -Population,fill=Gender)) + 
  scale_y_continuous(breaks=seq(-400,400,100),labels=abs(seq(-400,400,100))) + 
  geom_hline(yintercept = 0, lty = 1, lwd = 2) +
  scale_fill_manual(values = c('red','blue')) +
  coord_flip() +
  writR::graph_theme()

