#' generateAMId
#'
#' This function returns the in2data id for a set list of AMs.
#'
#' @import httr
#' @import dplyr
#' @export
generateAMId <- function(am,
                         in2data.url =
                           "https://in2data.qiagen.com/in2data/backend/api/"
){
  am_list <- NULL
  Token <- generateToken()
  for(i in 1:length(am)){
    
    reply <- httr::GET(paste0(in2data.url, paste0("am/search?name=", am[i])),
                       timeout(30),
                       httr::add_headers("Content-Type"="application/json",
                                         "Authorization"= Token)
    )
    
    Token <- unname(reply$request$headers["Authorization"])
    
    calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
    
    while(reply$status_code == 429){
      Sys.sleep(0.5)
      reply <- httr::GET(paste0(in2data.url, paste0("am/search?name=", am[i])),
                         timeout(30),
                         httr::add_headers("Content-Type"="application/json",
                                           "Authorization"= Token)
      )
      
      Token <- unname(reply$request$headers["Authorization"])
      
      calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
    }
    
    Sys.sleep(1/calls_remaining)
    
    am_id <- content(reply)
    am_id <- as.data.frame(am_id)
    am_list <- dplyr::bind_rows(am_list, data.frame(id = am_id$id,
                                                    am = am_id$name))
  }
  am_list <- am_list %>% filter(am %in% !!am)
  am_list %>% dplyr::mutate(query = paste0('{"id\": "', id, '"',
                                           ' ,"name": "', am, '"',
                                           '}'))
}