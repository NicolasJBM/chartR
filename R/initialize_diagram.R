#' @name initialize_diagram
#' @title Create a starting diagram
#' @author Nicolas Mangin
#' @description Function creating a basic diagram serving as a basis to be adjusted with the edit_diagram module.
#' @return A list of tibbles specifying nodes, relations, and moderations.
#' @importFrom tibble tibble
#' @export

initialize_diagram <- function(){
  
  nodes <- tibble::tibble(
    label = base::as.character(c("cause", "consequence", "moderator")),
    shape = base::factor(
      c("rectangle", "rectangle", "rectangle"),
      levels = c("ellipse","rectangle","diamond","egg","plaintext","triangle")
    ),
    x = c(0, 4, 2),
    y = c(0, 0, 1),
    width = 1,
    height = 0.5,
    penwidth = 1,
    color = base::as.character("black"),
    fillcolor = base::as.character("white"),
    fontsize = 12,
    fontcolor = base::as.character("black"),
    include = base::as.logical(TRUE)
  )
  
  relations <- tibble::tibble(
    relation = base::as.character("cause2consequence"),
    origin = base::factor(
      "cause",
      levels = base::unique(nodes$label)
    ),
    destination = base::factor(
      "consequence",
      levels = base::unique(nodes$label)
    ),
    style = base::factor("solid", levels = c("solid","dashed")),
    color = base::as.character("black"),
    fontcolor = base::as.character("black"),
    fontsize = 10,
    penwidth = 1,
    arrowtail = base::factor("none", levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
    arrowhead = base::factor("normal", levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
    label = base::as.character("increases"),
    include = base::as.logical(TRUE)
  )
  
  moderations <- tibble::tibble(
    origin = base::factor(
      "moderator",
      levels = base::unique(nodes$label)
    ),
    destination = base::factor(
      "cause2consequence",
      levels = unique(relations$relation)
    ),
    style = base::factor("solid", levels = c("solid","dashed")),
    color = base::as.character("black"),
    fontcolor = base::as.character("black"),
    fontsize = 10,
    penwidth = 1,
    arrowtail = base::factor("none", levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
    arrowhead = base::factor("normal", levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
    label = base::as.character("accentuates"),
    include = base::as.logical(TRUE)
  )
  
  diagram <- base::list(
    nodes = nodes,
    relations = relations,
    moderations = moderations
  )
  
  return(diagram)
}
