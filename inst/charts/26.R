library(VennDiagram) 

# Prepare the sets 
A <- c('A','B','C','D','E','F','G','H','I','J') 
B <- c('H','I','J','K','L','M','N','O','P','Q') 
C <- c('N','O','P','Q','R','S','A','B','I','J') 

ABC <- intersect(intersect(A,B),C) 
AB <- intersect(A,B) 
AC <- intersect(A,C) 
BC <- intersect(B,C) 

# Create the diagram 
venn.plot <- draw.triple.venn( 
  area1 = length(A), 
  area2 = length(B), 
  area3 = length(C), 
  n12 = length(AB), 
  n13 = length(AC), 
  n23 = length(BC), 
  n123 = length(ABC), 
  category = c('A', 'B', 'C'), 
  fill = c('dodgerblue', 'darkorange1', 'seagreen3'), 
  cat.col = c('dodgerblue', 'darkorange1', 'seagreen3'), 
  cat.cex = 2, 
  cex = 1, 
  margin = 0.05, 
  ind = TRUE,
  alpha = 0.25
)

# Replace the numbers with names 
venn.plot[[7]]$label <- paste0('\n\n', paste(setdiff(A, union(B,C)), collapse=' - '))
venn.plot[[8]]$label <- paste0('\n\n', paste(setdiff(AB,ABC), collapse=' - '))
venn.plot[[9]]$label <- paste0('\n\n', paste(setdiff(B, union(A,C)), collapse=' - '))
venn.plot[[10]]$label <- paste0('\n\n', paste(setdiff(AC,ABC), collapse=' - '))
venn.plot[[11]]$label <- paste0('\n\n', paste(ABC, collapse=' - '))
venn.plot[[12]]$label <- paste0('\n\n', paste(setdiff(BC,ABC), collapse=' - '))
venn.plot[[13]]$label <- paste0('\n\n', paste(setdiff(C, union(A,B)), collapse=' - '))

grid.draw(venn.plot) 
chart <- recordPlot()
