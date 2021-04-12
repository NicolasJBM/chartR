#' Gadget to interactively create a DiagrammeR object.
#' @return Tables for nodes, relations and moderations serving as input for draw_nomnet.
#' @seealso draw_nomnet
#' @param nodes_in Dataframe or tibble. Node description.
#' @param relations_in Dataframe or tibble. Relations description.
#' @param moderations_in Dataframe or tibble. Moderations description.
#' @import miniUI
#' @import shiny
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom DiagrammeR grVizOutput
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR render_graph
#' @importFrom rstudioapi insertText
#' @export


create_nomnet <- function(nodes_in = NA, relations_in = NA, moderations_in = NA) {
  ui <- miniPage(
    theme = paste0(find.package("bibliogR"),"css/boostrap.css"),
    
    gadgetTitleBar("Create a nomological network"),
    miniTabstripPanel(
      miniTabPanel(
        "Nodes",
        icon = icon("search"),
        rhandsontable::rHandsontableOutput("enter_nodes"),
        actionButton("update_nodes", "Update nodes"),
        tags$hr(),
        DiagrammeR::grVizOutput("draw_nodes")
      ),
      miniTabPanel(
        "Relations",
        icon = icon("search"),
        rhandsontable::rHandsontableOutput("enter_relations"),
        actionButton("update_relations", "Update relations"),
        tags$hr(),
        DiagrammeR::grVizOutput("draw_relations")
      ),
      miniTabPanel(
        "Moderations",
        icon = icon("search"),
        rhandsontable::rHandsontableOutput("enter_moderations"),
        actionButton("update_moderations", "Update moderations"),
        tags$hr(),
        DiagrammeR::grVizOutput("draw_moderations")
      )
    )
  )
  
  
  server <- function(input, output, session) {
    
    # Bind variables
    target <- NULL
    
    # Initialize tables
    
    tables <- reactiveValues()
    nodes <- reactive({
      if (suppressWarnings(is.na(nodes_in))){
        tibble::tibble(
          label = c("Ground", "Qualifier", "Claim", "Warrant", "Backing", "Rebuttal"),
          shape = factor(c("rectangle", "rectangle", "rectangle", "rectangle", "rectangle", "rectangle"), levels = c("ellipse","rectangle")),
          x = c(0, 2, 4, 1, 1, 4),
          y = c(0, 0, 0, 1, 2, 1),
          width = 1,
          height = 0.5,
          penwidth = 1,
          color = as.character("black"),
          fillcolor = as.character("white"),
          fontsize = 14,
          fontcolor = as.character("black"),
          include = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
        )
      } else nodes_in
    })
    observe({
      tables$nodes <- nodes()
    })
    
    
    relations <- reactive({
      if (suppressWarnings(is.na(relations_in))){
        tibble::tibble(
          relation = c("ground2qualifier","qualifier2claim","backing2warrant","rebuttal2claim"),
          source = factor(c("Ground","Qualifier","Backing","Rebuttal"), levels = unique(nodes()$label)),
          target = factor(c("Qualifier","Claim","Warrant","Claim"), levels = unique(nodes()$label)),
          style = factor(c("solid","solid","solid","solid"), levels = c("solid","dashed")),
          color = as.character("black"),
          fontcolor = as.character("black"),
          fontsize = 10,
          penwidth = 1,
          arrowhead = factor(c("normal","normal","normal","normal"), levels = c("normal","none")),
          label = as.character(c("observe","conclude","justify","challenge")),
          include = c(TRUE)
        )
      } else relations_in
    })
    observe({
      tables$relations <- relations()
    })
    
    
    moderations <- reactive({
      if (suppressWarnings(is.na(moderations_in))){
        tibble::tibble(
          source = factor("Warrant", levels = unique(nodes()$label)),
          target = factor("ground2qualifier", levels = unique(relations()$relation)),
          style = factor("solid", levels = c("solid","dashed")),
          color = as.character("black"),
          fontcolor = as.character("black"),
          fontsize = 10,
          penwidth = 1,
          arrowhead = factor("normal", levels = c("normal","none")),
          label = as.character("qualify"),
          include = c(TRUE)
        ) 
      } else moderations_in
    })
    observe({
      tables$moderations <- moderations()
    })
    
    
    
    # Create interface for nodes
    
    output$enter_nodes <- rhandsontable::renderRHandsontable({
      prep_nodes <- tables$nodes
      if (nrow(prep_nodes) == 0) prep_nodes[1,"color"] <- "black"
      rhandsontable::rhandsontable(prep_nodes, height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    observeEvent(input$update_nodes,{
      new_nodes <- suppressWarnings(rhandsontable::hot_to_r(input$enter_nodes))
      new_nodes <- new_nodes %>%
        dplyr::mutate_if(is.factor, as.character)
      
      new_relations <- tables$relations %>%
        dplyr::filter(
          source %in% unique(unlist(new_nodes$label)),
          target %in% unique(unlist(new_nodes$label))
        )
      
      new_moderations <- tables$moderations %>%
        dplyr::filter(
          source %in% unique(unlist(new_nodes$label)),
          target %in% unique(unlist(new_relations$relation))
        )
      
      tables$nodes <- new_nodes
      tables$relations <- new_relations
      tables$moderations <- new_moderations
    })
    
    output$draw_nodes <- DiagrammeR::renderGrViz({
      chartR::draw_nomnet(
        nodes = na.omit(tables$nodes),
        relations = na.omit(tables$relations),
        moderations = na.omit(tables$moderations)
      ) %>%
        DiagrammeR::render_graph()
    })
    
    
    # Create interface for relations
    
    output$enter_relations <- rhandsontable::renderRHandsontable({
      prep_relations <- tables$relations
      if (nrow(prep_relations) == 0) prep_relations[1,"color"] <- "black"
      labels <- unique(unlist(tables$nodes$label))
      prep_relations <- prep_relations %>%
        dplyr::mutate(
          source = factor(source, levels = labels),
          target = factor(target, levels = labels)
        )
      rhandsontable::rhandsontable(prep_relations, height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    observeEvent(input$update_relations,{
      new_relations <- suppressWarnings(rhandsontable::hot_to_r(input$enter_relations))
      labels <- unique(unlist(tables$nodes$label))
      
      new_relations <- new_relations %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::filter(source %in% labels, target %in% labels)
      
      new_moderations <- tables$moderations %>%
        dplyr::filter(
          source %in% unique(unlist(tables$nodes$label)),
          target %in% unique(unlist(new_relations$relation))
        )
      
      tables$relations <- new_relations
      tables$moderations <- new_moderations
    })
    
    output$draw_relations <- DiagrammeR::renderGrViz({
      chartR::draw_nomnet(
        nodes = na.omit(tables$nodes),
        relations = na.omit(tables$relations),
        moderations = na.omit(tables$moderations)
      ) %>%
        DiagrammeR::render_graph()
    })
    
    
    # Create interface for moderations
    
    output$enter_moderations <- rhandsontable::renderRHandsontable({
      prep_moderations <- tables$moderations
      if (nrow(prep_moderations) == 0) prep_moderations[1,"color"] <- "black"
      labels <- unique(unlist(tables$nodes$label))
      relations <- c("", unique(unlist(tables$relations$relation)))
      prep_moderations <- prep_moderations %>%
        dplyr::mutate(
          source = factor(source, levels = labels),
          target = factor(target, levels = relations)
        )
      rhandsontable::rhandsontable(prep_moderations, height = 400, width = "100%", stretchH = "all") %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    observeEvent(input$update_moderations,{
      new_moderations <- suppressWarnings(rhandsontable::hot_to_r(input$enter_moderations))
      labels <- unique(unlist(tables$nodes$label))
      relations <- unique(unlist(tables$relations$relation))
      
      new_moderations <- new_moderations %>%
        dplyr::filter(source %in% labels, target %in% relations)
      
      tables$moderations <- new_moderations
    })
    
    output$draw_moderations <- DiagrammeR::renderGrViz({
      chartR::draw_nomnet(
        nodes = na.omit(tables$nodes),
        relations = na.omit(tables$relations),
        moderations = na.omit(tables$moderations)
      ) %>%
        DiagrammeR::render_graph()
    })
    
    
    observeEvent(input$done, {
      
      nodes <- tables$nodes %>%
        na.omit() %>%
        dplyr::mutate_if(is.factor, as.character)
      
      relations <- tables$relations %>%
        na.omit() %>%
        dplyr::mutate_if(is.factor, as.character)
      
      moderations <- tables$moderations %>%
        na.omit() %>%
        dplyr::mutate_if(is.factor, as.character)
      
      c(
        write_table_code(nodes, "nodes"),
        write_table_code(relations, "relations"),
        write_table_code(moderations, "moderations")
      ) %>%
        rstudioapi::insertText()
      
      stopApp()
    })
    
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
