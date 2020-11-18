#' get_techlog
#
#' Returns a dataframe containing the techlog values for a set of IDs:
#' @param cartridges: Dataframe containing the id to use
#' @param in2data.url: url of the in2data API
#'
#' @import httr
#' @import dplyr
#' @import tidyr
#' @import jsonlite
#' 
# By UIA & OE on 26/08/2020

#' @export
#' 
get_techlog <- function(cartridges,
                            in2data.url = 
                              "https://in2data.qiagen.com/in2data/backend/api/"){
  
  
  i = 0
  
  Token <- generateToken()
  
  for(cartridge_id in unique(cartridges$id)){
    i = i + 1
    
    reply <- GET(paste0(in2data.url, "/results/tech/log/",
                        cartridge_id),
                 timeout(30),
                 add_headers("Content-Type"="application/json", 
                             "Authorization"= Token), 
                 body = '')
    
    Token <- unname(reply$request$headers["Authorization"])
    
    calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
    
    while(reply$status_code == 429){
      Sys.sleep(0.5)
      reply <- GET(paste0(in2data.url, "/results/tech/log/",
                          name),
                   timeout(30),
                   add_headers("Content-Type"="application/json", 
                               "Authorization"= Token), 
                   body = '')
      
      Token <- unname(reply$request$headers["Authorization"])
      
      calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
    }
    
    Sys.sleep(1/calls_remaining)
    
    if (is.null(content(reply)$code)){
      
      cartridges$calibration[cartridges$id == cartridge_id] <- 
        list(fromJSON(content(reply, as = 'text', encoding = 'UTF-8'),
                      flatten = TRUE) %>% 
               splitstackshape::cSplit(., names(.)) %>% 
               select_if(~any(!is.na(.))) %>% 
               mutate_all(~gsub('c[(]|[)]', '', .)) %>% 
               mutate_all(~case_when(. == 'NULL' ~ NA_character_,
                                     TRUE ~ .))
        )
      
      print(paste0(cartridge_id, ' --> PROCESSED    ',
                   i, ' out of ', length(unique(cartridges$id))))
    } else{
      cartridges$calibration[cartridges$id == cartridge_id] <- list(NA)
      print(paste0(cartridge_id, ' --> DOES NOT HAVE TECHLOG    ',
                   i, ' out of ', length(unique(cartridges$id))))
      
    }
    
  }
  
  cartridges <- cartridges %>%
    unnest(calibration) %>%
    nest(techlog = -id)
  
  return(cartridges)
}
