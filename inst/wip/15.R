gg.gauge <- function(pos,breaks=c(0,30,70,100)) {
  library(ggplot2)
  get.poly <- function(a,b,r1=0.3,r2=1) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  maincol <- case_when(
    pos <= breaks[2] ~ "red",
    pos <= breaks[3] ~ "orange",
    pos <= breaks[4] ~ "yellow",
    pos <= breaks[5] ~ "green",
    TRUE ~ "forestgreen"
  )
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="orange")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="yellow")+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="green")+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(breaks[1],pos,0.75,1), aes(x,y),fill=maincol, alpha = 1)+
    geom_polygon(data=get.poly(breaks[1],pos,0.3,0.325), aes(x,y),fill="grey21", alpha = 0.8)+
    geom_polygon(data=get.poly(breaks[1],pos,0.725,0.75), aes(x,y),fill="grey21", alpha = 0.8)+
    geom_polygon(data=get.poly(breaks[1],pos,0.975,1), aes(x,y),fill="grey21", alpha = 0.8)+
    geom_polygon(data=get.poly(pos,breaks[6]), aes(x,y),fill="grey31", alpha = 1)+
    geom_polygon(data=get.poly(pos-1,pos+1,0.1,1.05),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks)))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

chart <- gg.gauge(65,breaks=c(0,20,40,60,80,100))
