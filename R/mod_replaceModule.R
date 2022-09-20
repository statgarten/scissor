#' @title Shiny Module for data transformation
#' @description replaceModule UI Function
#'
#'
#' @param id id of module
#'
#'
#' @import shiny
#' @export
mod_replaceModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = ns("from"),
          label = "A",
          value = "",
          placeholder = "this value will be"
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = ns("to"),
          label = "X",
          value = "",
          placeholder = "changed to this"
        )
      )
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}
#' @title Shiny Module for data transformation
#' @description replaceModule Server Functions
#' @param id id of module
#' @param inputData "reactive" data
#' @return transformed data
#' @export
#' @import shiny
#'
mod_replaceModule_server <- function(id, inputData) {
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

    output$description <- renderText({
      paste(
        "A -> X",
        "[A, B, C, A, B, C] -> [X, B, C, X, B, C]",
        sep = "\n"
      )
    })

    data_replaced <- reactive({
      req(inputData())
      data <- inputData()

      data <- scissor::impute(
        inputData = data,
        column = input$cols,
        operator = "Replace",
        value = input$from,
        to = input$to
      )

      data
    })

    return(data_replaced)
  })
}

## To be copied in the UI
# mod_replaceModule_ui("replaceModule_1")

## To be copied in the server
# mod_replaceModule_server("replaceModule_1")
