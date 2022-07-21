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
            "base::subset(", column, " != ", value, ")"
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
            "base::transform(", column, " = ifelse(",column, " == ", value, ", ", to, ", ", column, "))"
          )
      ))
    }
  }


}
