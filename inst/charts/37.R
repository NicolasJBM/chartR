library(networkDynamic)
library(ndtv)
library(dplyr)
library(network)
library(tidyr)
library(tidygraph)
library(intergraph)


vertices <- tibble::tibble(
  name = as.character(c(1,2,3,4,5)),
  firstname = c("Valentina","Brice","John","Maria","Anja"),
  group = c(1,1,2,2,2)
)

timenet <- data.frame(
  period = c(1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5,5,5),
  from   = c(1,2,3,1,2,3,4,2,3,4,2,3,4,5,1,2,3,4,5),
  to     = c(2,3,4,2,5,4,5,1,5,3,1,2,3,4,2,3,1,3,1),
  weight = c(2,1,2,2,1,2,1,2,1,2,2,1,2,1,2,1,1,2,1)
) %>%
  tidyr::spread(from, weight, fill = 0) %>%
  tidyr::gather(from, weight, -to, -period) %>%
  tidyr::spread(to, weight, fill = 0) %>%
  group_by(period) %>%
  nest() %>%
  mutate(
    data = purrr::map(
      data,
      function(x) tidygraph::as_tbl_graph(as.matrix(tibble::column_to_rownames(tibble::remove_rownames(x), "from")))
    )
  ) %>%
  mutate(data = purrr::map(data, tidygraph::activate, "nodes")) %>%
  mutate(data = purrr::map(data, function(x,y) dplyr::left_join(x,y, by = "name"), vertices)) %>%
  mutate(data = purrr::map(data, function(x) mutate(x, degree = tidygraph::centrality_degree(mode = "all")))) %>%
  mutate(data = purrr::map(data, function(x) intergraph::asNetwork(tidygraph::as.igraph(x))))

timenet <- as.list(timenet$data)

movie <- networkDynamic(
  base.net = timenet[[1]],
  network.list=timenet,
  onsets = seq(from = 0, to = (length(timenet) - 1)),
  termini = seq(from = 1, to = length(timenet)),
  verbose = FALSE
)


compute.animation(
  movie,
  animation.mode = "kamadakawai",
  slice.par=list(start=0, end=4, interval=1, aggregate.dur=1, rule='any'),
  verbose = FALSE) 

chart <- render.d3movie(movie, usearrows = FALSE,  
                        displaylabels = TRUE, 
                        label = "firstname", 
                        bg="#ffffff", 
                        vertex.border="#333333", 
                        vertex.cex = timenet[[5]] %v% "degree",   
                        vertex.col = timenet[[5]] %v% "group", 
                        edge.lwd = 1,  
                        edge.col = '#55555599', 
                        launchBrowser=TRUE, 
                        render.par=list(tween.frames = 30, show.time = F), 
                        plot.par=list(mar=c(0,0,0,0)), output.mode='html', 
                        verbose = FALSE) 

