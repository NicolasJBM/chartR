#' @name update_diagram
#' @title Update a diagram
#' @author Nicolas Mangin
#' @description Function updating a diagram serving based on user input.
#' @param nodes       Tibble. Nodes with their properties.
#' @param relations   Tibble. Edges with their properties.
#' @param moderations Tibble. Moderations with their properties.
#' @return A list of tibbles specifying nodes, relations, and moderations.
#' @importFrom dplyr mutate
#' @export

update_diagram <- function(nodes, relations, moderations){
  
  arrowhead <- NULL
  shape <- NULL
  style <- NULL
  target <- NULL
  include <- NULL
  relation <- NULL
  
  nodes <- nodes |>
    dplyr::mutate(
      shape = base::factor(
        shape,
        levels = c("ellipse","rectangle","diamond","egg","plaintext","triangle")
      ),
      include = base::as.logical(include)
    )
  
  relations <- relations |>
    dplyr::mutate(
      relation = base::as.character(relation),
      source = base::factor(
        source,
        levels = base::unique(nodes$label)
      ),
      target = base::factor(
        target,
        levels = base::unique(nodes$label)
      ),
      style = base::factor(style, levels = c("solid","dashed")),
      arrowhead = base::factor(arrowhead, levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
      include = base::as.logical(include)
    )
  
  moderations <- moderations |>
    dplyr::mutate(
      source = base::factor(
        source,
        levels = base::unique(nodes$label)
      ),
      target = base::factor(
        target,
        levels = unique(relations$relation)
      ),
      style = base::factor(style, levels = c("solid","dashed")),
      arrowhead = base::factor(arrowhead, levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
      include = base::as.logical(include)
    )
  
  diagram <- base::list(
    nodes = nodes,
    relations = relations,
    moderations = moderations
  )
  
  return(diagram)
}
