#' @title Split data frame's column text
#' @description apply column split text and provides 2 result column. Column provided as character
#' @examples
#' iris %>% split(column = 'Sepal.Length', splitby = '.')
#' @param inputData data frame to split
#' @param column column that split will be applied
#' @param splitby keyword to used as split; ex. '.'
#' @return data frame with newly splited columns
#' @seealso tidyr's `separate` function
#' @import magrittr
#' @importFrom tidyr separate
#' @export
#'
split <- function(inputData, column, splitby){
  if(splitby == '.'){
    splitby = '\\\\.' # using eval & parse . \ needs to be \\
  }
  eval(parse(
    text =
      paste0(
        "inputData <- inputData %>% ",
        "tidyr::separate(", column, ", sep = '", splitby, "', into = c('",column , "A", "','", column, "B", "'), fill = 'right')"
      )
  ))
  return(inputData)
}

