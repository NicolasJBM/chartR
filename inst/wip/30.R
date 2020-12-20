
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,5), sep=""))
d2=data.frame(from=rep(d1$to, each=5), to=paste("subgroup", seq(1,25), sep="_"))
edges=rbind(d1, d2)

# create a dataframe with connection between leaves (individuals)
all_leaves=paste("subgroup", seq(1,25), sep="_")
connect=rbind(
  data.frame(from=sample(all_leaves, 25, replace=T), to=sample(all_leaves, 25, replace=T)),
  data.frame(from=sample(all_leaves[1:5], 10, replace=T), to=sample(all_leaves[21:25], 5, replace=T)),
  data.frame(from=sample(all_leaves[1:5], 10, replace=T) , to=sample( all_leaves[11:15], 5, replace=T)),
  data.frame(from=sample(all_leaves[16:20], 10, replace=T) , to=sample( all_leaves[21:25], 5, replace=T)) )
connect$value=runif(nrow(connect))

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(31)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]

####################

#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)



####################

# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices=vertices)
# The connection object must refer to the ids of the leaves:
from = match( connect$from, vertices$name)
to = match( connect$to, vertices$name)


####################

chart <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=2, aes(colour=..index..)) +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.2, y=y*1.2, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=4, alpha=1) +
  
  geom_node_point(aes(filter = leaf, x = x*1.1, y=y*1.1, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(5,15) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) +
  expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5))

