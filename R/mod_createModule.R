#' @title Shiny Module for feature create
#' @description createModule UI Function
#'
#' @param id id of module
#'
#' @import shiny
#' @export
mod_createModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 6,
      textInput(
        inputId = ns("newName"),
        label = "new column Name",
        value = ""
      )
    ),
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("operator"),
          label = "operator",
          choices = c('+', '-', '*', '/', '=', 'Paste')
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("Column")
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("byColumn")
        )
      )
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}
#' @title Shiny Module for feature creation
#' @description createModule Server Functions
#' @param id id of module
#' @param inputData "reactive" data
#' @return transformed data
#' @export
#' @import shiny
#'
mod_createModule_server <- function(id, inputData) {
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

    output$byColumn <- renderUI({
      selectInput(
        inputId = ns("by"),
        label = "by Column",
        choices = colnames(inputData()),
        multiple = FALSE
      )
    })

    output$description <- renderText({
      paste(
        "A -> The",
        "['A banana', 'A Grape', 'ABCD'] -> ['The banana', 'The Grape', 'TheBCD']",
        sep = "\n"
      )
    })

    data_replaced <- reactive({
      req(inputData())
      data <- inputData()

      if(input$operator == '='){
        additive = data[,input$cols]
      }

      if(input$operator == '+'){
        additive = data[, input$cols] + data[, input$by]
      }

      if(input$operator == '-') {
        additive = data[, input$cols] - data[, input$by]
      }

      if(input$operator == '*') {
        additive = data[, input$cols] * data[, input$by]
      }

      if(input$operator == '/') {
        additive = data[, input$cols] / data[, input$by]
      }

      if(input$operator == 'Paste') {
        additive = paste0(data[, input$cols], data[, input$by])
      }

      data <- cbind(data, additive)
      colnames(data)[ncol(data)]  <- input$newName

      data
    })

    return(data_replaced)
  })
}

## To be copied in the UI
# mod_createModule_ui("createModule_1")

## To be copied in the server
# mod_createModule_server("createModule_1")
