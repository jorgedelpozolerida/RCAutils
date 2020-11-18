#' lot_number
#'
#' This function replaces X00000 Lot with the one obtained through SN.
#'
#'
# By Lucía & José on 26/02/2020

#' @export
lot_number <- function(SN){
  new_lot <- dplyr::case_when(grepl('^49|48|47', SN) ~ 
                                paste0(1,
                                       substring(SN, 2, 2),
                                       0,
                                       substring(SN, 3, 5)),
                              grepl('^40', SN) ~ 
                                paste0(20, substring(SN, 2, 5)),
                            TRUE ~ substring(SN, 1, 6))

  return(new_lot)
}
