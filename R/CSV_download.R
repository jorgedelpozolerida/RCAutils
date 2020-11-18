#' CSV_download
#
#' Downloads CSVs from in2data using the following inputs:
#' @param cartridges: List containing the id to use
#' @param path: file.path() of the output folder
#' @param in2data.url: url of the in2data API
#' @param use_cache: Boolean determining whether the cache will be used or not
#'
#' @import httr
#' @import dplyr
#' @import tidyr
#' 
# By Lucía & José on 08/04/2020

#' @export
CSV_download <- function(cartridges,
                         in2data.url = 
                           "https://in2data.qiagen.com/in2data/backend/api/",
                         use_cache = TRUE){
  
  
  i = 0
  
  Token <- generateToken()
  
  for(cartridge_id in unique(cartridges$id)){
    path <- cartridges$path[cartridges$id == cartridge_id]
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    name <- cartridges$name[cartridges$id == cartridge_id]
    i = i + 1
    filename = file.path(path,
                         paste0(name, '.csv'))
    
    
    if(!file.exists(filename) | !use_cache){
      
      reply <- GET(paste0(in2data.url, "results/raw/download/CSV/",
                          cartridge_id),
                   timeout(30),
                   add_headers("Content-Type"="application/json", 
                               "Authorization"= Token), 
                   body = '')
      
      Token <- unname(reply$request$headers["Authorization"])
      
      calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
      
      while(reply$status_code == 429){
        Sys.sleep(0.5)
        reply <- GET(paste0(in2data.url, "results/raw/download/CSV/",
                            cartridge_id),
                     timeout(30),
                     add_headers("Content-Type"="application/json", 
                                 "Authorization"= Token), 
                     body = '')
        
        Token <- unname(reply$request$headers["Authorization"])
        
        calls_remaining <- as.numeric(reply$headers$`x-rate-limit-remaining`) + 1
      }
      
      Sys.sleep(1/calls_remaining)
      
      if (!is.list(content(reply))){
        writeBin(content(reply), filename)
        print(paste0(name, '.csv --> COPIED    ',
                     i, ' out of ', length(unique(cartridges$id))))
        cartridges$exist[cartridges$id == cartridge_id] <- TRUE
      }else{
        print(paste0(name, '.csv --> DOES NOT EXIST    ',
                     i, ' out of ', length(unique(cartridges$id))))
        cartridges$exist[cartridges$id == cartridge_id] <- FALSE
      }
    } else{
      print(paste0(name, '.csv --> ALREADY EXISTS    ',
                   i, ' out of ', length(unique(cartridges$id))))
      cartridges$exist[cartridges$id == cartridge_id] <- TRUE
    }
  }
  
  return(cartridges)
}
