#' na_as_0
#'
#' This function replaces NA with 0s.
#'
#' @export
na_as_0 <- function(x){
  # Checks the class of a a variable and turns any empty char to an NA
  if('factor' %in% class(x)) x <- as.character(x)
  ifelse(!is.na(as.character(x)), x, '0')
}