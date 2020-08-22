library(ggplot2)
library(rayshader)
#library(av)

faithful_dd <- ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  ggtitle("3D Plotting in R from 2D_ggplot_graphics") +
  labs(caption = "Package: rayshader") +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))

plot_gg(faithful_dd, multicore = TRUE, width = 4, height = 4, scale = 100, zoom = 0.6, phi = 60, background = "#afceff", shadowcolor = "#3a4f70")

#render_movie("faithful_3d.mp4", frames = 100)
