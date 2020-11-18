#' na_as_empty
#'
#' This function replaces NA with empty cells.
#'
# By Lucía & José on 06/03/2020

#' @export
na_as_empty <- function(x){
  # Checks the class of a a variable and turns any empty char to an NA
  if('factor' %in% class(x)) x <- as.character(x)
  ifelse(!is.na(as.character(x)), x, '')
}