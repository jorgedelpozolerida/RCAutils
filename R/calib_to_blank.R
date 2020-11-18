#' calib_to_blank
#'
#' This function extracts the blanking matrix matrix from calibration data.
#'
# By UIA & OE on 21/07/2020

#' @export
calib_to_blank <- function(x) {
  if (is.atomic(x)) {
    return(data.frame(Sensor = NA, RC1 = 0))
  } else{
    x <- matrix(unlist(x$blankMeasurement),
                nrow = 6,
                ncol = 8,
                byrow = TRUE,
                dimnames = list(seq(0, 5, by = 1),
                                c('RC1', 'RC2', 'RC3', 'RC4', 
                                  'RC5', 'RC6', 'RC7', 'RC8')))
    return(as.data.frame(x) %>%
             tibble::rownames_to_column(var = 'Sensor'))
  }
}