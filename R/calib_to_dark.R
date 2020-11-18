#' calib_to_dark
#'
#' This function extracts the blanking matrix matrix from calibration data.
#'
# By UIA & OE on 21/07/2020

#' @export
calib_to_dark <- function(x) {
  if (is.atomic(x)) {
    return(data.frame(S0 = NA))
  } else{
    x <- matrix(unlist(x$darkMeasurement),
                nrow = 1,
                ncol = 6,
                dimnames = list('1',
                                c('S0', 'S1', 'S2', 'S3', 'S4', 'S5')))
    return(as.data.frame(x))
  }
}
