#' @name draw_diagram
#' @title Create a diagram
#' @author Nicolas Mangin
#' @description Function creating a diagram object from tibbles specifying nodes, relations, and moderations.
#' @param nodes       Tibble. Nodes with their properties.
#' @param relations   Tibble. Edges with their properties.
#' @param moderations Tibble. Moderations with their properties.
#' @param translations Tibble. Translations for the nodes labels.
#' @param language Character. Language to display.
#' @return A DiagrammeR object ready for rendering.
#' @importFrom DiagrammeR add_edge
#' @importFrom DiagrammeR add_node
#' @importFrom DiagrammeR create_graph
#' @importFrom DiagrammeR edge_aes
#' @importFrom DiagrammeR node_aes
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom tibble rowid_to_column
#' @importFrom stringr str_replace_all
#' @export

draw_diagram <- function(nodes, relations, moderations, translations, language) {

  # Bind variables
  from_x <- NULL
  from_y <- NULL
  include <- NULL
  label <- NULL
  node_id <- NULL
  relation <- NULL
  relation_id <- NULL
  origin <- NULL
  destination <- NULL
  to_x <- NULL
  to_y <- NULL
  x <- NULL
  y <- NULL
  altlabel <- NULL
  
  # Necessary information to tables for connections and create graph:
  
  translate <- translations[,c("label", language)]
  base::names(translate) <- c("label","altlabel")
  translatenodes <- dplyr::select(translate, label, altlabel)
  translateorig <- dplyr::select(translate, origin = label, altlabel)
  translatedest <- dplyr::select(translate, destination = label, altlabel)
  
  nodes <- nodes |>
    dplyr::mutate_if(base::is.factor, base::as.character) |>
    dplyr::mutate_if(base::is.logical, base::as.character) |>
    dplyr::left_join(translatenodes, by = "label") |>
    dplyr::mutate(label = altlabel) |>
    dplyr::select(-altlabel)
  
  relations <- relations |>
    dplyr::mutate_if(base::is.factor, base::as.character) |>
    dplyr::mutate_if(base::is.logical, base::as.character) |>
    dplyr::left_join(translateorig, by = "origin") |>
    dplyr::mutate(origin = altlabel) |>
    dplyr::select(-altlabel)
  
  relations <- relations |>
    dplyr::mutate_if(base::is.factor, base::as.character) |>
    dplyr::mutate_if(base::is.logical, base::as.character) |>
    dplyr::left_join(translatedest, by = "destination") |>
    dplyr::mutate(destination = altlabel) |>
    dplyr::select(-altlabel)
  
  moderations <- moderations |>
    dplyr::mutate_if(base::is.factor, base::as.character) |>
    dplyr::mutate_if(base::is.logical, base::as.character) |>
    dplyr::left_join(translateorig, by = "origin") |>
    dplyr::mutate(origin = altlabel) |>
    dplyr::select(-altlabel)
  
  nodes <- nodes |>
    dplyr::filter(include == TRUE) |>
    tibble::rowid_to_column("node_id")

  
  if (base::nrow(relations) > 0) {
    
    relations <- relations |>
      dplyr::filter(include == TRUE) |>
      tibble::rowid_to_column(var = "relation_id") |>
      dplyr::left_join(
        dplyr::select(nodes, origin = label, from = node_id, from_x = x, from_y = y),
        by = "origin"
      ) |>
      dplyr::left_join(
        dplyr::select(nodes, destination = label, to = node_id, to_x = x, to_y = y),
        by = "destination"
      ) |>
      dplyr::mutate(
        by = nrow(nodes) + relation_id,
        by_x = (from_x + to_x) / 2,
        by_y = (from_y + to_y) / 2
      )
  }

  if (nrow(moderations) > 0) {
    moderations <- moderations |>
      dplyr::filter(include == TRUE) |>
      dplyr::left_join(
        dplyr::select(nodes, origin = label, from = node_id),
        by = "origin"
      ) |>
      dplyr::left_join(
        dplyr::select(relations, destination = relation, to = by),
        by = "destination"
      ) |>
      dplyr::select(-origin, -destination)
  }
  
  graph <- DiagrammeR::create_graph()


  # Add the nodes:

  for (i in base::seq_len(base::nrow(nodes))) {
    graph <- DiagrammeR::add_node(
      graph,
      label = nodes$label[[i]],
      node_aes = DiagrammeR::node_aes(
        shape = nodes$shape[[i]],
        x = nodes$x[[i]],
        y = nodes$y[[i]],
        width = nodes$width[[i]],
        height = nodes$height[[i]],
        penwidth = nodes$penwidth[[i]],
        color = nodes$color[[i]],
        fillcolor = nodes$fillcolor[[i]],
        fontsize = nodes$fontsize[[i]],
        fontcolor = nodes$fontcolor[[i]]
      )
    )
  }

  # Add the junctions between nodes:

  if (nrow(relations) > 0){
    for (i in base::seq_len(base::nrow(relations))) {
      graph <- DiagrammeR::add_node(
        graph,
        label = relations$by[[i]],
        node_aes = DiagrammeR::node_aes(
          x = relations$by_x[[i]],
          y = relations$by_y[[i]],
          width = 0,
          height = 0,
          penwidth = 0,
          color = "#00000000",
          fillcolor = "#00000000",
          fontsize = 0,
          fontcolor = "#00000000"
        )
      )
    }
  }

  # Add the relationships:
  
  if (nrow(relations) > 0){
    for (i in base::seq_len(base::nrow(relations))) {
      graph <- DiagrammeR::add_edge(
        graph = graph,
        from = relations$from[[i]],
        to = relations$by[[i]],
        edge_aes = DiagrammeR::edge_aes(
          style = relations$style[[i]],
          color = relations$color[[i]],
          fontcolor = relations$fontcolor[[i]],
          fontsize = relations$fontsize[[i]],
          penwidth = relations$penwidth[[i]],
          dir = "both",
          arrowtail = relations$arrowtail[[i]],
          arrowhead = "none",
          label = relations$label[[i]]
        )
      )
      graph <- DiagrammeR::add_edge(
        graph = graph,
        from = relations$by[[i]],
        to = relations$to[[i]],
        edge_aes = DiagrammeR::edge_aes(
          style = relations$style[[i]],
          color = relations$color[[i]],
          penwidth = relations$penwidth[[i]],
          dir = "both",
          arrowtail = "none",
          arrowhead = relations$arrowhead[[i]],
          label = ""
        )
      )
    }
  }

  # Add the moderations:

  if (nrow(moderations) > 0) {
    for (i in base::seq_len(base::nrow(moderations))) {
      graph <- DiagrammeR::add_edge(
        graph = graph,
        from = moderations$from[[i]],
        to = moderations$to[[i]],
        edge_aes = DiagrammeR::edge_aes(
          style = moderations$style[[i]],
          color = moderations$color[[i]],
          fontcolor = moderations$fontcolor[[i]],
          fontsize = moderations$fontsize[[i]],
          penwidth = moderations$penwidth[[i]],
          dir = "both",
          arrowtail = moderations$arrowtail[[i]],
          arrowhead = moderations$arrowhead[[i]],
          label = moderations$label[[i]]
        )
      )
    }
  }

  return(graph)
}
