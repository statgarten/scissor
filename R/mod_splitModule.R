#'
#' @title Shiny Module for data transform
#' @description splitModule UI Function
#'
#' @param id id of module
#' @import shiny
#' @export
#'
mod_splitModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    textInput(
      inputId = ns("keyword"),
      label = "",
      placeholder = "/"
    ),
    h4("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}


#' @description splitModule Server Functions
#' @title Shiny Module for data transform
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#' @return rounded data (not reactive)
#' @export
#'
mod_splitModule_server <- function(id, inputData, opened) {
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
      paste0("[COL] 12/34 -> [COLA] 12, [COLB] 34. \nSpecial character require \\\\ ex) '\\\\.'")
    })

    data_splited <- reactive({
      req(inputData())

      data <- scissor::split(
        inputData = inputData(),
        column = input$cols,
        splitby = input$keyword
      )
      data
    })

    return(data_splited)
    # observeEvent(input$splitButton, {
    #   inputData(
    #     scissor::split(
    #       inputData = inputData(),
    #       column = input$splitColumn,
    #       splitby = input$splitKeyword
    #     )
    #   )
    #   updateSelectizeInput(
    #     session,
    #     inputId = "splitColumn",
    #     label = "splitSelectLabel",
    #     choices = colnames(inputData()),
    #     server = TRUE
    #   )
    # })
  })
}
