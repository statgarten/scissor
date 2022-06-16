#' @import magrittr
#' @export
#'
binarize <- function(inputData, column, operator, value){
  if (operator %in% c(">", ">=", "<", "<=", "==", "!=")) {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, "bin = ",
          "ifelse( ", column, " ", operator, " ", value, ", 1, 0 ) )"
        )
    ))
  }

  if (operator == "In") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, "bin = ",
          "ifelse( ", column, " %in% ", value, ", 1, 0 ) )"
        )
    ))
  }
  if (operator == "Not In") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, "bin = ",
          "ifelse(! ", column, " %in% ", value, ", 1, 0 ) )"
        )
    ))
  }
  if (operator == "Contains") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, "bin = ",
          "ifelse( grepl(", column, ", ", value, "), 1, 0 ) )"
        )
    ))
  }
  if (operator == "Not Contains") {
    eval(parse(
      text =
        paste0(
          "inputData <- inputData %>% ",
          "base::transform( ", column, "bin = ",
          "ifelse(! grepl(", column, ", ", value, "), 1, 0 ) )"
        )
    ))
  }

  return(inputData)
}
