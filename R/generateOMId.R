#' generateOMId
#'
#' This function returns the in2data id for a set list of OMs.
#'
#' @import httr
#' @import dplyr
# By Lucía & José on 26/02/2020
#

#' @export
generateOMId <- function(om,
                         in2data.url =
                           "https://in2data.qiagen.com/in2data/backend/api/"
){
  om_list <- NULL
  
  Token <- generateToken()
  
  for(i in 1:length(om)){
    reply <- httr::GET(paste0(in2data.url, paste0("om/name/", om[i])),
                       timeout(30),
                       httr::add_headers("Content-Type" = "application/json",
                                         "Authorization" = Token),
                       body = {})
    
    Token <- unname(reply$request$headers["Authorization"])
    
    calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
    
    while(reply$status_code == 429){
      Sys.sleep(0.5)
      reply <- httr::GET(paste0(in2data.url, paste0("om/name/", om[i])),
                         timeout(30),
                         httr::add_headers("Content-Type" = "application/json",
                                           "Authorization" = Token),
                         body = {})
      
      Token <- unname(reply$request$headers["Authorization"])
      
      calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
    }
    
    Sys.sleep(1/calls_remaining)

    om_id <- content(reply)
    om_list <- dplyr::bind_rows(om_list, data.frame(id = om_id$id,
                                                    om = om_id$name))
  }

  om_list %>% dplyr::mutate(query = paste0('{"id\": "', id, '"',
                                           ' ,"name": "', om, '"',
                                           '}'))
}
