library(tibble)
library(dplyr)
library(ggplot2)
library(plotly)

chart <- plotly::plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "euros",
  node = list(
    label = c("First","Second","Third","Fourth","Fifth","Sixth"),
    color =  c("red", "blue", "green", "purple", "yellow", "orange"),
    pad = 15,
    thickness = 15,
    line = list(
      color = "grey",
      width = 2
    )
  ),
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(8,4,2,8,4,2),
    label =  c("A","B","C","D","E","F")
  )
) %>% 
  plotly::layout(
    title = "Sankey Diagram",
    font = list(
      size = 10,
      color = 'white'
    ),
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    plot_bgcolor = 'black',
    paper_bgcolor = 'black'
  )
