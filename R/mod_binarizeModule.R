#'
#' @title Shiny Module for data transform
#' @description binarizeModule UI Function
#'
#' @param id id of module
#' @import shiny
#' @export
#'
mod_binarizeModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      outputId = ns("Column")
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("operator"),
          label = "",
          choice = c(">", ">=", "<", "<=", "==", "!=", "In", "Not In", "Contains", "Not Contains"),
          selected = ">",
          multiple = FALSE
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = ns("value"),
          label = "",
          placeholder = "2"
        )
      )
    ),
    h5("Example"),
    verbatimTextOutput(
      ns("description")
    )
  )
}

#' @description binarizeModule Server Functions
#' @title Shiny Module for data transform
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#' @return rounded data (not reactive)
#' @export
#'
mod_binarizeModule_server <- function(id, inputData) {
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
        "x > 2",
        "[0, 1, 2, 3, 4, 5, 6] -> [0, 0, 0, 1, 1, 1, 1]",
        "# In requires value with c() : ",
        "ex: c('X', 'Y', 'Z') ",
        sep = "\n"
      )
    })

    data_binarize <- reactive({
      req(inputData())
      data <- inputData()

      data <- binarize(
        inputData = data,
        column = input$cols,
        operator = input$operator,
        value = input$value
      )

      data
    })

    return(data_binarize)
  })
}
