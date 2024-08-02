#' @name update_diagram
#' @title Update a diagram
#' @author Nicolas Mangin
#' @description Function updating a diagram serving based on user input.
#' @param nodes       Tibble. Nodes with their properties.
#' @param relations   Tibble. Edges with their properties.
#' @param moderations Tibble. Moderations with their properties.
#' @param translations Tibble. Translations for the nodes labels.
#' @return A list of tibbles specifying nodes, relations, and moderations.
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @export

update_diagram <- function(nodes, relations, moderations, translations){
  
  arrowtail <- NULL
  arrowhead <- NULL
  shape <- NULL
  style <- NULL
  origin <- NULL
  destination <- NULL
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
      origin = base::factor(
        origin,
        levels = base::unique(nodes$label)
      ),
      destination = base::factor(
        destination,
        levels = base::unique(nodes$label)
      ),
      style = base::factor(style, levels = c("solid","dashed")),
      arrowtail = base::factor(arrowtail, levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
      arrowhead = base::factor(arrowhead, levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
      include = base::as.logical(include)
    )
  
  moderations <- moderations |>
    dplyr::mutate(
      origin = base::factor(
        origin,
        levels = base::unique(nodes$label)
      ),
      destination = base::factor(
        destination,
        levels = unique(relations$relation)
      ),
      style = base::factor(style, levels = c("solid","dashed")),
      arrowtail = base::factor(arrowtail, levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
      arrowhead = base::factor(arrowhead, levels = c("none","normal","diamond","dot","inv","vee","tee","box","crow","curve")),
      include = base::as.logical(include)
    )
  
  moderations <- moderations |>
    dplyr::mutate_all(base::as.character())
  
  diagram <- base::list(
    nodes = nodes,
    relations = relations,
    moderations = moderations,
    translations = translations
  )
  
  return(diagram)
}
