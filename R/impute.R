#' @title Impute data frame's data
#' @description Remove or Replace specific value by given keyword
#' @examples
#' impute(iris, 'Sepal.Width', 'Replace', value = '3.1', to = '3.12345')
#' impute(iris, 'Species', 'Remove', value = 'virginica')
#' @param inputData data frame
#' @param column column to be funcion applied
#' @param opeartor either 'Remove' or 'Replace'
#' @param value keyword to remove or replace
#' @param to if operator is "Replace", target to be transformed
#' @return data frame with changed column
#' @seealso dplyr's `filter_` and `mutate_` function
#' @import magrittr
#' @export
#'

impute <- function(inputData, column, operator, value = NULL, to = NULL){
  if(operator == 'Remove'){
    if(is.null(value)){
      eval(parse(
        text =
          paste0(
            "inputData <- inputData %>% ",
            "base::subset(!is.na(", column, "))"
          )
      ))
    } else {
      eval(parse(
        text =
          paste0(
            "inputData <- inputData %>% ",
            "base::subset(", column, " != '", value, "')"
          )
      ))
    }

  }

  if(operator == 'Replace'){
    to <- ifelse(is.numeric(to), to, paste0('"', to, '"'))
    if(is.null(value)){
      eval(parse(
        text =
          paste0(
            "inputData <- inputData %>% ",
            "base::transform(", column, " = ifelse(is.na(",column, "), ", to, ", ", column, "))"
          )
      ))
    } else{
      eval(parse(
        text =
          paste0(
            "inputData <- inputData %>% ",
            "base::transform(", column, " = ifelse(",column, " == '", value, "', ", to, ", ", column, "))"
          )
      ))
    }
  }

  return(inputData)
}
