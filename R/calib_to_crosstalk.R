#' calib_to_crosstalk
#'
#' This function extracts the direct crosstalk matrix from calibration data.
#'
# By UIA & OE on 16/07/2020

#' @export
calib_to_crosstalk <- function(x) {
  if (is.atomic(x)) {
    return(data.frame(Affected = NA))
  } else{
 
    x <- matrix(MASS::ginv(
      matrix(
        unlist(x$inverseCrosstalk),
        nrow = 6,
        ncol = 6,
        byrow = TRUE
      )
    ),
    nrow = 6,
    ncol = 6,
    dimnames = list(
      c('S0', 'S1', 'S2', 'S3', 'S4', 'S5'),
      c('F0', 'F1', 'F2', 'F3', 'F4', 'F5')
    ))
    
    return(as.data.frame(x) %>%  tibble::rownames_to_column(var = 'Affected'))
  }
}
