#' @import magrittr
#' @export
#'

subset <- function(inputData, column, operator, value){
  if (operator == "In") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::subset( ", column, " %in% ","c(", value, ") )"
        )
    ))
  }
  if (operator == "Not In") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::subset( !", column, " %in% ","c(", value, ") ) "
        )
    ))
  }
  if (operator == "Contains") {
    eval(parse(
      text =
        paste0(
          "inputData <-", "inputData %>% ",
          "base::subset(grepl(", value, ", ", column,") )"
        )
    ))
  }
  if (operator == "Not Contains") {
    eval(parse(
      text =
        paste0(
          "inputData <-", "inputData %>% ",
          "base::subset( ! grepl(", value, ", ", column, ") ) "
        )
    ))
  }

  if (operator %in% c(">", ">=", "<", "<=", "==", "!=")) {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::subset(", column, " ", operator," ", value, ")"
        )
    ))
  }

  return(inputData)
}
