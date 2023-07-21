#'
#' @title Shiny Module for data transform
#' @description roundModule UI Function
#'
#' @param id id of module
#' @import shiny
#' @export
#'
mod_roundModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    sliderInput(
      inputId = ns("num"),
      label = "Round Digits",
      min = -5,
      max = 5,
      value = 0,
      step = 1
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}

#' @description roundModule Server Functions
#' @title Shiny Module for data transform
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#' @return rounded data (not reactive)
#' @export
mod_roundModule_server <- function(id, inputData) {
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
      paste0("12345.56789 -> ", round(12345.56789, digits = input$num))
    })

    data_rounded <- reactive({
      req(inputData())
      data <- inputData()

      data <- trans(
        inputData = data,
        column = input$cols,
        operator = "Round",
        value = input$num
      )

      data
    })

    return(data_rounded)
  })
}
