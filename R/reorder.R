#' @title Reorder column of Dataframe
#' @description select data as given column's order. Column provided as character.
#' @examples
#' reorder(iris, c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
#' @param inputData data frame to reorder
#' @param columns vector of column names
#' @return data frame with column order changed
#' @seealso dplyr's `select_` function
#' @import magrittr
#' @export
#'
reorder <- function(inputData, columns) {
  inputData <- inputData %>%
    base::subset(select = columns)
  return(inputData)
}
