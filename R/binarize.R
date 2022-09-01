#' @title binarize dataframe's column
#' @description change column as 1 or 0 with provided criteria
#' @examples
#' scissor::binarize(iris,'Sepal.Length', '>', '6')
#' scissor::binarize(iris,'Species', 'In', 'c("virginica", "setosa")')
#'
#' @param inputData data frame
#' @param column column to be function applied
#' @param operator ">", ">=", "<", "<=", "==", "!=", "In", "Not In", "Contains", "Not Contains"
#' @param value value for used as criteria. in Case of In / Contains value should give as `c('EX1','EX2','EX3')` format
#'
#' @return data frame with changed column
#' @seealso dplyr's `mutate_` function
#' @import magrittr
#' @export
#'
binarize <- function(inputData, column, operator, value){
  if (operator %in% c(">", ">=", "<", "<=", "==", "!=")) {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = ",
          "ifelse( ", column, " ", operator, " '", value, "', 1, 0 ) )"
        )
    ))
  }

  if (operator == "In") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = ",
          "ifelse( ", column, " %in% ", value, ", 1, 0 ) )"
        )
    ))
  }
  if (operator == "Not In") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = ",
          "ifelse(! ", column, " %in% ", value, ", 1, 0 ) )"
        )
    ))
  }
  if (operator == "Contains") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = ",
          "ifelse( grepl(", column, ", '", value, "'), 1, 0 ) )"
        )
    ))
  }
  if (operator == "Not Contains") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = ",
          "ifelse(! grepl(", column, ", '", value, "'), 1, 0 ) )"
        )
    ))
  }

  return(inputData)
}
