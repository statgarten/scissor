#' @title transform (numerically) dataframe's column
#' @description apply column with round, log, normalize.
#' @examples
#' scissor::trans(iris,'Sepal.Length', '-')
#' scissor::trans(iris,'Sepal.Length', 'round')
#'
#' @param inputData data frame
#' @param column column to be function applied (should be numerical column)
#' @param operator "round", "Log", "Log10", "Log2", "Sqrt", "-", "Min-Max", "Normal"
#' @param value value for used as additional parameter (round only for now)
#'
#' @return data frame with changed column
#' @seealso dplyr's `mutate_` function
#' @import magrittr
#' @export
#'
trans <- function(inputData, column, operator, value = NULL){
  if(operator == 'Round'){
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = round(", column, ", ", value, "))"
        )
    ))
  }

  if (operator %in% c("Log", "Log10", "Log2", "Sqrt", "-" ) ) {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = ", tolower(operator), "(", column,"))"
        )
    ))
  }

  if (operator == "Min-Max") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform(",column, " = minmax(", column, "))"
        )
    ))
  }

  if (operator == "Normal") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform(",column, " = normalize(", column, "))"
        )
    ))
  }

  return(inputData)
}


minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalize <- function(x) {
  (x - mean(x)) / sd(x)
}
