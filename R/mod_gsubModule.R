#' @title Shiny Module for subtext replace
#' @description gsubModule UI Function
#'
#' @param id id of module
#'
#' @import shiny
#' @export
mod_gsubModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = ns("pattern"),
          label = "Pattern",
          value = "",
          placeholder = "this value will be"
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = ns("replacement"),
          label = "Replacement",
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
#' @title Shiny Module for sub text replace
#' @description gsubModule Server Functions
#' @param id id of module
#' @param inputData "reactive" data
#' @return transformed data
#' @export
#' @import shiny
#'
mod_gsubModule_server <- function(id, inputData) {
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
        "A -> The",
        "['A banana', 'A Grape', 'ABCD'] -> ['The banana', 'The Grape', 'TheBCD']",
        sep = "\n"
      )
    })

    data_replaced <- reactive({
      req(inputData())
      data <- inputData()

      data[, input$cols] <- gsub(
        pattern = input$pattern,
        replacement = input$replacement,
        x = unlist(data[, input$cols], use.names = FALSE)
      )
      data
    })

    return(data_replaced)
  })
}

## To be copied in the UI
# mod_gsubModule_ui("gsubModule_1")

## To be copied in the server
# mod_gsubModule_server("gsubModule_1")
