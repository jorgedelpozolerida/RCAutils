#' empty_as_0
#'
#' This function replaces empty cells with 0.
#'
# By UÍA & OE on 16/06/2020

#' @export
empty_as_0 <- function(x){
  # Checks the class of a a variable and turns any empty char to an NA
  if('factor' %in% class(x)) x <- as.character(x)
  ifelse(as.character(x)!="", x, '0')
}