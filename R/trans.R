#' @import magrittr
#' @export
#'

trans <- function(inputData, column, operator){
  if(operator == 'Round'){
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, " = round(", column, "))"
        )
    ))
  }

  if (operator %in% c("Log", "Log10", "Sqrt", "-" ) ) {
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
