

#' @importFrom DiagrammeR renderDiagrammeR
#' @importFrom DiagrammeR DiagrammeROutput
#' @importFrom DiagrammeR render_graph
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR grVizOutput


output$grviz <- renderGrViz({
  validate(need(rendering() == "grviz", message=FALSE))
  source(paste0(find.package("writR"), "/charts/",afterid()$idchart[[1]],".R"))
  if (exists("chart")) get("chart")
})