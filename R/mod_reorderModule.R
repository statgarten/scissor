#'
#' @title Shiny Module for data transform
#' @description reorderModule UI Function
#'
#' @param id id of module
#' @import shiny
#' @export
mod_reorderModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("Sortable")
    )
  )
}

#' @description reorderModule Server Functions
#' @title Shiny Module for data transform
#' @import shiny
#' @importFrom sortable rank_list
#' @param id id of module
#' @param inputData "reactive" data
#' @return rounded data (not reactive)
#' @export

mod_reorderModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$Sortable <- renderUI({
      rank_list(
        text = "Columns",
        labels = colnames(inputData()),
        input_id = ns("columns"),
        class = c("default-sortable") # add custom style
      )
    })

    data_reorder <- reactive({
      req(inputData())
      data <- inputData()

      data <- reorder(
        inputData = data,
        column = input$columns
      )

      data
    })

    return(data_reorder)
  })
}
