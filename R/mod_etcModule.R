#' @title Shiny Module for data transformation
#' @description etcModlue UI Function
#'
#'
#' @param id id of module
#' @export
#' @import shiny
#'
#'
mod_etcModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    radioButtons(
      inputId = ns("type"),
      label = "Type of transformation",
      choices = c("-", "Sqrt", "Min-Max", "Normal"),
      selected = "-",
      inline = TRUE
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}

#' @title Shiny Module for data transformation
#' @description etcModlue Server Functions
#' @export
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#' @return transformed data
mod_etcModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Column <- renderUI({
      selectInput(
        inputId = ns("cols"),
        label = "on Column",
        choices = colnames(inputData()),
        multiple = FALSE
      )
    })

    observeEvent(input$type, {
      if (input$type == "-") {
        output$description <- renderText({
          "-(10) -> -10"
        })
      }
      if (input$type == "Sqrt") {
        output$description <- renderText({
          "sqrt(4) -> 2"
        })
      }

      if (input$type == "Min-Max") {
        output$description <- renderText({
          "[1, 3, 2, 5, 4] -> [0, 0.5, 0.25, 1, 0.75]"
        })
      }

      if (input$type == "Normal") {
        output$description <- renderText({
          "[1, 3, 2, 5, 4] -> [-1.26, 0, -0.63, 1.26, 0.63]"
        })
      }
    })


    data_transed <- reactive({
      req(inputData())
      data <- inputData()

      data <- trans(
        inputData = data,
        column = input$cols,
        operator = input$type
      )

      data
    })

    return(data_transed)
  })
}

## To be copied in the UI
# mod_etcModlue_ui("etcModlue_1")

## To be copied in the server
# mod_etcModlue_server("etcModlue_1")
