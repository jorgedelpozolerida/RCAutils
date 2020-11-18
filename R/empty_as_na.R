#' empty_as_na
#'
#' This function replaces empty cells with NA.
#'
# By Lucía & José on 26/02/2020

#' @export
empty_as_na <- function(x){
  # Checks the class of a a variable and turns any empty char to an NA
  if('factor' %in% class(x)) x <- as.character(x)
  ifelse(as.character(x)!="", x, NA)
}
