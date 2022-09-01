#' @title subset dataframe's column
#' @description subset data with given criteria
#' @examples
#' iris %>% scissor::subset('Sepal.Length', '>', '5')
#' iris %>% scissor::subset('Species', 'In', 'c("virginica", "setosa")')
#'
#' @param inputData data frame
#' @param column column to be function applied
#' @param operator "In", "Not In", "Contains", "Not Contains", ">", ">=", "<", "<=", "==", "!="
#' @param value value for used as criteria. in Case of In / Contains value should give as `c('EX1','EX2','EX3')` format
#'
#' @return data frame with changed column
#' @seealso dplyr's `filter_` function, scissor's `binarize` function.
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
