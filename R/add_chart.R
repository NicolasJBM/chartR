#' Gadget to insert various examples of charts in documents.
#' @return A citation.
#' @import miniUI
#' @import shiny
#' @import shinythemes
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyOutput
#' @importFrom collapsibleTree renderCollapsibleTree
#' @importFrom collapsibleTree collapsibleTreeOutput
#' @importFrom ndtv ndtvAnimationWidgetOutput
#' @importFrom ndtv renderNdtvAnimationWidget
#' @importFrom rstudioapi insertText
#' @export


add_chart <- function() {
  ui <- miniPage(
    theme = shinytheme("spacelab"),

    gadgetTitleBar("Insert chart"),
    miniTabstripPanel(
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          fluidRow(
            column(4, uiOutput("information")),
            column(4, uiOutput("object")),
            column(4, uiOutput("type"))
          ),
          fluidRow(
            column(4, uiOutput("format")),
            column(4, uiOutput("chartid")),
            column(4, checkboxInput("inchunk", "In chunk", value = TRUE))
          ),
          tags$hr(),
          uiOutput("display"),
          tags$hr(),
          fluidRow(
            column(3, actionButton("insert", "Insert")),
            column(9, textOutput("use"))
          )
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Bind variables
    idchart <- NULL
    information <- NULL
    object <- NULL
    type <- NULL
    render <- NULL


    ##############################################################################
    ####                          FILTERS                                     ####
    ##############################################################################

    ##########################
    output$information <- renderUI({
      choices <- c("", sort(unique(dplyr::filter(chartR::charts, !is.na(render))$information)))
      shiny::selectInput("slctinfo", "Information:", choices = choices, selected = "")
    })

    afterinfo <- reactive({
      if (is.null(input$slctinfo)) {
        dplyr::filter(chartR::charts, !is.na(render))
      } else if (input$slctinfo == "") {
        dplyr::filter(chartR::charts, !is.na(render))
      } else {
        dplyr::filter(chartR::charts, !is.na(render)) %>%
          dplyr::filter(information == input$slctinfo)
      }
    })

    ##########################
    output$object <- renderUI({
      choices <- c("", sort(unique(afterinfo()$object)))
      shiny::selectInput("slctobj", "Object:", choices = choices, selected = "")
    })

    afterobject <- reactive({
      if (is.null(input$slctobj)) {
        afterinfo()
      } else if (input$slctobj == "") {
        afterinfo()
      } else {
        afterinfo() %>%
          dplyr::filter(object == input$slctobj)
      }
    })

    ##########################
    output$type <- renderUI({
      choices <- sort(na.omit(unique(c("", aftertype()$type))))
      shiny::selectInput("slcttype", "Type:", choices = choices, selected = "")
    })

    aftertype <- reactive({
      if (is.null(input$slcttype)) {
        afterobject()
      } else if (input$slcttype == "") {
        afterobject()
      } else {
        afterobject() %>%
          dplyr::filter(type %in% c("any", input$slcttype))
      }
    })

    ##########################
    output$format <- renderUI({
      choices <- sort(na.omit(unique(c("", aftertype()$format))))
      shiny::selectInput("slctfmt", "Format:", choices = choices, selected = "")
    })

    afterformat <- reactive({
      if (is.null(input$slctfmt)) {
        aftertype()
      } else if (input$slctfmt == "") {
        aftertype()
      } else {
        aftertype() %>%
          dplyr::filter(format %in% c("any", input$slctfmt))
      }
    })

    ##########################
    output$chartid <- renderUI({
      choices <- sort(na.omit(unique(c(0, afterformat()$idchart))))
      shiny::selectInput("slctid", "Chart:", choices = choices, selected = "")
    })

    afterid <- reactive({
      if (is.null(input$slctid)) {
        afterformat()
      } else if (input$slctid == 0) {
        afterformat()
      } else {
        afterformat() %>%
          dplyr::filter(idchart == input$slctid)
      }
    })



    ##############################################################################
    ####                          DISPLAY                                     ####
    ##############################################################################

    rendering <- reactive({
      if (!is.null(afterid())) {
        if (nrow(afterid()) == 1) {
          afterid()$render[[1]]
        } else {
          "text"
        }
      } else {
        "text"
      }
    })

    ##########################
    output$plot <- renderPlot({
      validate(need(rendering() == "plot", message = FALSE))
      source(paste0(find.package("chartR"), "/charts/", afterid()$idchart[[1]], ".R"))
      if (exists("chart")) get("chart")
    })

    output$plotly <- renderPlotly({
      validate(need(rendering() == "plotly", message = FALSE))
      source(paste0(find.package("chartR"), "/charts/", afterid()$idchart[[1]], ".R"))
      if (exists("chart")) get("chart")
    })

    output$collapsibleTree <- renderCollapsibleTree({
      validate(need(rendering() == "collapsibleTree", message = FALSE))
      source(paste0(find.package("chartR"), "/charts/", afterid()$idchart[[1]], ".R"))
      if (exists("chart")) get("chart")
    })

    output$ndtv <- ndtv::renderNdtvAnimationWidget({
      validate(need(rendering() == "ndtv", message = FALSE))
      source(paste0(find.package("chartR"), "/charts/", afterid()$idchart[[1]], ".R"))
      if (exists("chart")) get("chart")
    })

    output$text <- renderText({
      "Please select one type of chart to display."
    })


    ##########################
    output$display <- renderUI({
      if (rendering() == "plot") {
        plotOutput("plot", height = "400px")
      } else if (rendering() == "plotly") {
        plotlyOutput("plotly", height = "400px")
      } else if (rendering() == "collapsibleTree") {
        collapsibleTreeOutput("collapsibleTree", height = "400px")
      } else if (rendering() == "ndtv") {
        ndtv::ndtvAnimationWidgetOutput("ndtv", width = "100%", height = "400px")
      } else {
        textOutput("text")
      }
    })


    output$use <- renderText({
      if (!is.null(afterid())) {
        if (nrow(afterid()) == 1) {
          afterid()$use
        } else {
          ""
        }
      } else {
        ""
      }
    })

    ##############################################################################
    ####                           INSERT                                     ####
    ##############################################################################

    observeEvent(input$insert, {
      if (rendering() == "text") {
        code <- "\n\n\n"
      } else {
        code <- paste(c(readLines(paste0(find.package("chartR"), "/charts/", afterid()$idchart[[1]], ".R")), "\nchart"), "\n")
      }

      if (input$inchunk) {
        if (afterid()$type[[1]] == "static" & afterid()$format[[1]] == "latex") {
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else if (afterid()$type[[1]] == "static" & afterid()$format[[1]] == "html") {
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else if (afterid()$type[[1]] == "interactive" & afterid()$format[[1]] == "html") {
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else if (afterid()$type[[1]] == "dynamic" & afterid()$format[[1]] == "html") {
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else {
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        }
      }

      code %>%
        paste0(collapse = "") %>%
        rstudioapi::insertText()
    })

    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
