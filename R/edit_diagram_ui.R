#' @name edit_diagram_ui
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation of diagrams embedded in functions or documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Write lines of codes creating the different parts of a diagram which can then be embedded in function or document.
#' @importFrom DiagrammeR grVizOutput
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @importFrom shinydashboardPlus box
#' @export


edit_diagram_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::actionButton(
          ns("applychanges"), "Apply changes", icon = shiny::icon("rotate"),
          style = "background-color:#000066;color:#FFF;width:100%;
          margin-top:10px;margin-bottom:25px;"
        ),
        shinydashboardPlus::box(
          title = "Nodes", status = "navy",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("minimize"),
          background = NULL, gradient = FALSE,
          rhandsontable::rHandsontableOutput(ns("editnodes"))
        ),
        shinydashboardPlus::box(
          title = "Relations", status = "primary",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("right-left"),
          background = NULL, gradient = FALSE,
          rhandsontable::rHandsontableOutput(ns("editrelations"))
        ),
        shinydashboardPlus::box(
          title = "Moderations", status = "info",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("turn-down"),
          background = NULL, gradient = FALSE,
          rhandsontable::rHandsontableOutput(ns("editmoderations"))
        )
      ),
      shiny::column(
        6,
        shiny::actionButton(
          ns("clear"), "Clear", icon = shiny::icon("trash"),
          style = "background-color:#660000;color:#FFF;width:100%;
          margin-top:10px;margin-bottom:25px;"
        ),
        shinydashboardPlus::box(
          title = "Diagram", status = "teal",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("diagram-project"),
          background = NULL, gradient = FALSE,
          DiagrammeR::grVizOutput(ns("displaydiagram"))
        ),
        shinydashboardPlus::box(
          title = "Code", status = "success",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("code"),
          background = NULL, gradient = FALSE,
          shiny::uiOutput(ns("displaycode"))
        )
      )
    )
  )
}

