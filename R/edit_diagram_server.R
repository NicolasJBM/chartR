#' @name edit_diagram_server
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation of diagrams embedded in functions or documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param diagramfolder Character. Path to the folder which may contain diagrams.
#' @return Write lines of codes creating the different parts of a diagram which can then be embedded in function or document.
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR render_graph
#' @importFrom readxl read_excel
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom writexl write_xlsx
#' @importFrom shinyalert shinyalert
#' @export


edit_diagram_server <- function(id, diagramfolder = base::getwd()){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    # Initialize ###############################################################
    
    arrowhead <- NULL
    arrowtail <- NULL
    color <- NULL
    destination <- NULL
    fillcolor <- NULL
    ontcolor <- NULL
    fontsize <- NULL
    fontcolor <- NULL
    height <- NULL
    include <- NULL
    label <- NULL
    origin <- NULL
    penwidth <- NULL
    relation <- NULL
    shape <- NULL
    style <- NULL
    width <- NULL
    x <- NULL
    y <- NULL
    
    modrval <- shiny::reactiveValues()
    
    output$diagram <- shiny::renderUI({
      shiny::req(diagramfolder)
      if (base::dir.exists(diagramfolder)){
        files <- base::list.files(diagramfolder)
        diagrams <- files[stringr::str_detect(files, "^diagram_")]
        diagrams <- base::unique(stringr::str_remove_all(diagrams, "^diagram_|_nodes.xlsx$|_relations.xlsx$|_moderations.xlsx$"))
        
      } else diagrams <- c()
      modrval$diagrams <- c("newdiagram", diagrams)
      modrval$selection <- "newdiagram"
      modrval$nodes <- NA
      modrval$relations <- NA
      modrval$moderations <- NA
      shiny::selectInput(ns("diagram"), "Diagram:", choices = modrval$diagrams, selected = modrval$selection, width = "100%")
    })
    
    shiny::observeEvent(input$load, {
      if (input$diagram == "newdiagram"){
        diagram <- chartR::initialize_diagram()
        modrval$nodes <- diagram$nodes
        modrval$relations <- diagram$relations
        modrval$moderations <- diagram$moderations
      } else {
        modrval$nodes <- readxl::read_excel(
          path = base::paste0(diagramfolder, "/diagram_", input$diagram, "_nodes.xlsx"),
          sheet = "nodes"
        ) |>
          dplyr::mutate(
            label = base::as.character(label),
            shape = base::as.character(shape),
            x = base::as.numeric(x),
            y = base::as.numeric(y),
            width = base::as.numeric(width),
            height = base::as.numeric(height),
            penwidth = base::as.numeric(penwidth),
            color = base::as.character(color),
            fillcolor = base::as.character(fillcolor),
            fontsize = base::as.numeric(fontsize),
            fontcolor = base::as.character(fontcolor),
            include = base::as.logical(include)
          )
        modrval$relations <- readxl::read_excel(
          path = base::paste0(diagramfolder, "/diagram_", input$diagram, "_relations.xlsx"),
          sheet = "relations"
        ) |>
          dplyr::mutate(
            relation = base::as.character(relation),
            origin = base::factor(origin, levels = modrval$nodes$label),
            destination = base::factor(destination, levels = modrval$nodes$label),
            style = base::as.character(style),
            color = base::as.character(color),
            fontcolor = base::as.character(fontcolor),
            fontsize = base::as.numeric(fontsize),
            penwidth = base::as.numeric(penwidth),
            arrowtail = base::as.character(arrowtail),
            arrowhead = base::as.character(arrowhead),
            label = base::as.character(label),
            include = base::as.logical(include)
          )
        modrval$moderations <- readxl::read_excel(
          path = base::paste0(diagramfolder, "/diagram_", input$diagram, "_moderations.xlsx"),
          sheet = "moderations"
        ) |>
          dplyr::mutate(
            origin = base::factor(origin, levels = modrval$nodes$label),
            destination = base::factor(destination, levels = modrval$relations$relation),
            style = base::as.character(style),
            color = base::as.character(color),
            fontcolor = base::as.character(fontcolor),
            fontsize = base::as.numeric(fontsize),
            penwidth = base::as.numeric(penwidth),
            arrowtail = base::as.character(arrowtail),
            arrowhead = base::as.character(arrowhead),
            label = base::as.character(label),
            include = base::as.logical(include)
          )
      }
    })
    
    
    
    shiny::observeEvent(input$save, {
      shiny::showModal(shiny::modalDialog(
        title = "Diagram","",
        shiny::textInput(ns("diagname"), "Name of the diagram:", value = input$diagram, width = "100%"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirmsave"), "OK")
        )
      ))
    })
    
    
    shiny::observeEvent(input$confirmsave, {
      shiny::removeModal()
      shiny::req(!base::is.na(modrval$nodes))
      shiny::req(!base::is.na(modrval$relations))
      shiny::req(!base::is.na(modrval$moderations))
      nodes <- modrval$nodes
      writexl::write_xlsx(
        base::list(nodes = nodes),
        base::paste0(diagramfolder, "/diagram_", input$diagname, "_nodes.xlsx")
      )
      relations <- modrval$relations
      writexl::write_xlsx(
        base::list(relations = relations),
        base::paste0(diagramfolder, "/diagram_", input$diagname, "_relations.xlsx")
      )
      moderations <- modrval$moderations
      writexl::write_xlsx(
        base::list(moderations = moderations),
        base::paste0(diagramfolder, "/diagram_", input$diagname, "_moderations.xlsx")
      )
      shinyalert::shinyalert(
        title = "Diagram saved!",
        text = "Reload the course to see it appear in the diagram selection list.",
        type = "success", closeOnEsc = FALSE
      )
    })
    
    
    
    # Edit #####################################################################
    
    output$editnodes <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.na(modrval$nodes))
      modrval$nodes |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", stretchH = "all"
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    output$editrelations <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.na(modrval$relations))
      modrval$relations |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", stretchH = "all"
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    output$editmoderations <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.na(modrval$moderations))
      modrval$moderations |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", stretchH = "all"
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$refresh, {
      shiny::req(input$editnodes)
      shiny::req(input$editrelations)
      shiny::req(input$editmoderations)
      nodes <- rhandsontable::hot_to_r(input$editnodes)
      relations <- rhandsontable::hot_to_r(input$editrelations)
      moderations <- rhandsontable::hot_to_r(input$editmoderations)
      diagram <- chartR::update_diagram(nodes, relations, moderations)
      modrval$nodes <- diagram$nodes
      modrval$relations <- diagram$relations
      modrval$moderations <- diagram$moderations
    })
    
    
    
    # Display ##################################################################
    
    output$displaydiagram <- DiagrammeR::renderGrViz({
      shiny::req(!base::is.na(modrval$nodes))
      shiny::req(!base::is.na(modrval$relations))
      shiny::req(!base::is.na(modrval$moderations))
      
      shinybusy::show_modal_spinner(
        spin = "cube-grid",
        color = "firebrick",
        text = "Generating the diagram..."
      )
      
      chart <- chartR::draw_diagram(
        nodes = modrval$nodes,
        relations = modrval$relations,
        moderations = modrval$moderations
      ) |>
        DiagrammeR::render_graph()
      
      shinybusy::remove_modal_spinner()
      
      chart
    })
    
    output$displaycode <- shiny::renderUI({
      shiny::req(!base::is.na(modrval$nodes))
      shiny::req(!base::is.na(modrval$relations))
      shiny::req(!base::is.na(modrval$moderations))
      code = c(
        "",
        chartR::write_table_code(modrval$nodes, "nodes"),
        "",
        chartR::write_table_code(modrval$relations, "relations"),
        "",
        chartR::write_table_code(modrval$moderations, "moderations"),
        "",
        "chartR::draw_diagram(nodes, relations, moderations) |>",
        "  DiagrammeR::render_graph()"
      )
      shinyAce::aceEditor(
        outputId = ns("diagramcode"), value = code, mode = "r",
        wordWrap = TRUE, debounce = 10, autoComplete = "live", height = "700px"
      )
    })
  
  })
}

