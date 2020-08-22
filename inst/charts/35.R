library(tibble)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(tidyr)

response <-function(x,y) {-10.4+6.53*x+6.53*y-0.167*x^2-0.167*y^2+0.0500*x*y}
  
x <- seq(1,10,0.01)
y <- seq(1,10,0.01)

response <- expand.grid(x=x,y=y) %>%
  mutate(z = response(x,y)) %>%
  spread(y,z) %>%
  column_to_rownames("x") %>%
  as.matrix()

chart <- plot_ly(
  x = x,
  y = y,
  z = response,
  type = "surface"
)
