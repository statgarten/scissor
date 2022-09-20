#'
#' @title Shiny Module for data transform
#' @description exportModule UI Function
#'
#' @param id id of module
#' @import shiny
#' @export
#'
#'
mod_exportModule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("filename"),
      label = "filename"
    ),
    selectInput(
      inputId = ns("ext"),
      label = "file extension",
      selected = ".csv",
      multiple = FALSE,
      choices = c(".csv", ".rda", ".sqlite", ".xlsx")
    ),
    downloadButton(
      outputId = ns("exportButton"),
      label = "export",
      style = "width: 100%"
    )
  )
}

#' @description exportModule Server Functions
#' @title Shiny Module for data transform
#' @import shiny
#' @importFrom sortable rank_list
#' @param id id of module
#' @param inputData "reactive" data
#' @return rounded data (not reactive)
#' @export
#' @importFrom writexl write_xlsx
#' @importFrom RSQLite dbWriteTable dbConnect dbDriver dbDisconnect

mod_exportModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$exportButton <- downloadHandler(
      filename = function() {
        if (input$filename == "") {
          return(paste0("downloaded", input$ext))
        }
        return(paste0(input$filename, input$ext))
      },
      content = function(con) {
        if (input$ext == ".csv") {
          write.csv(inputData(), file = con, row.names = FALSE)
        }
        if (input$ext == ".rda") {
          saveRDS(inputData(), file = con)
        }
        if (input$ext == ".sqlite") {
          RSQLite::dbWriteTable(
            conn = RSQLite::dbConnect(
              RSQLite::dbDriver("SQLite"),
              dbname = con
            ),
            name = "data1",
            value = inputData()
          )
          RSQLite::dbDisconnect()
          # con <- dbConnect(drv=RSQLite::SQLite(), dbname="SQLITE_FILENAME")
          # data <- dbGetQuery(conn=con, statement = "SELECT * FROM 'data1'")
        }
        if (input$ext == ".xlsx") {
          writexl::write_xlsx(inputData(), path = con)
        }
      }
    )
  })
}
