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

