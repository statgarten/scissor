#' @import magrittr
#' @export
#'
drop <- function(inputData, column){
  eval(parse(
    text =
      paste0(
        "inputData <- inputData %>% ",
        "base::subset(select = -", column, ")"
      )
  ))
  return(inputData)
}

