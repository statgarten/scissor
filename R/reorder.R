#' @import magrittr
#' @export
#'
reorder <- function(inputData, columns){
  inputData <- inputData %>% base::subset(select = columns)
  return(inputData)
}
