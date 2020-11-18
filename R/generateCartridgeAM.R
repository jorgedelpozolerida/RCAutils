#' generateCartridgeOM
#'
#' This function returns the metadata for an AM under the UIA db format.
#'
#' @import httr
#' @import dplyr
# By Lucía & José on 26/02/2020
#


#' @export
generateCartridgeAM <- function(am,
                                in2data.url =
                                  "https://in2data.qiagen.com/in2data/backend/api/",
                                fromTime = '1980-01-01'
){
  
  fromTime <- format(as.Date(fromTime), '%Y-%m-%dT%H:%M:%S.000Z')
  
  body <- generateAMId(am)
  
  Token <- generateToken()
  
  results.page <- POST(paste0(in2data.url, "results/0/50"),
                       timeout(30),
                       add_headers("Content-Type"="application/json",
                                   "Authorization"= Token),
                       body = paste0('{"filters":{"amName":{
                     "value":[', paste(body$query, collapse = ', '), ']',
                                     ', "operation":"EQ"},',
                                     '"startTimestamp":{"value":{"from":"',
                                     fromTime, '"',
                                     '},"operation":"EQ"}',
                                     '}}'))
  
  calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 1
  
  while(results.page$status_code == 429){
    Sys.sleep(0.5)
    results.page <- POST(paste0(in2data.url, "results/0/50"),
                         timeout(30),
                         add_headers("Content-Type"="application/json",
                                     "Authorization"= Token),
                         body = paste0('{"filters":{"amName":{
                     "value":[', paste(body$query, collapse = ', '), ']',
                                       ', "operation":"EQ"},',
                                       '"startTimestamp":{"value":{"from":"',
                                       fromTime, '"',
                                       '},"operation":"EQ"}',
                                       '}}'))
    
    Token <- unname(results.page$request$headers["Authorization"])
    
    calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 1
  }
  
  Sys.sleep(1/calls_remaining)
  
  tests <- NULL
  
  n_pages <- content(results.page)$numberOfPages - 1
  
  for (page in seq(0, n_pages)){
    
    results.page <- POST(paste0(in2data.url, paste0('results/',
                                                    page,
                                                    '/50')),
                         timeout(30),
                         add_headers("Content-Type"="application/json",
                                     "Authorization"= Token),
                         body = paste0('{"filters":{"amName":{
                     "value":[', paste(body$query, collapse = ', '), ']',
                                       ', "operation":"EQ"},',
                                       '"startTimestamp":{"value":{"from":"',
                                       fromTime, '"',
                                       '},"operation":"EQ"}',
                                       '}}'))
    
    Token <- unname(results.page$request$headers["Authorization"])
    
    calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 1
    
    while(results.page$status_code == 429){
      Sys.sleep(0.5)
      results.page <- POST(paste0(in2data.url, paste0('results/',
                                                      page,
                                                      '/50')),
                           timeout(30),
                           add_headers("Content-Type"="application/json",
                                       "Authorization"= Token),
                           body = paste0('{"filters":{"amName":{
                     "value":[', paste(body$query, collapse = ', '), ']',
                                         ', "operation":"EQ"},',
                                         '"startTimestamp":{"value":{"from":"',
                                         fromTime, '"',
                                         '},"operation":"EQ"}',
                                         '}}'))
      
      Token <- unname(results.page$request$headers["Authorization"])
      
      calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 1
    }
    
    Sys.sleep(1/calls_remaining)
    
    test.page <- content(results.page)$content %>%
      lapply(function(x){
        x %>%
          unlist() %>%
          as.data.frame() %>%
          t() %>%
          as.data.frame()
      }) %>%
      bind_rows() %>%
      mutate_all(as.character) %>%
      mutate(uid = case_when(cartridgeSerialNumber == 'P00000007' ~
                               sampleIdentifier,
                             TRUE ~  cartridgeSerialNumber)
      ) %>%
      rename(idi2d = id,
             cartridge_SN = cartridgeSerialNumber,
             sample_id = sampleIdentifier,
             start_time = startTimestamp,
             AM = amName,
             OM = omName,
             ADF_name = assayName,
             ADF_version = assayVersion,
             error_code = errorCode,
             IC = controlsPassed,
             lot = lotNumber,
             sample_type = sampleType.name,
             total_result = totalResult) %>%
      mutate(total_result = case_when(total_result == 'POSITIVE' ~
                                        'pos',
                                      total_result == 'POSITIVE_WITH_WARNINGS' ~
                                        'pos*',
                                      total_result == 'NEGATIVE' ~
                                        'neg',
                                      total_result == 'FAILED' ~
                                        'fail',
                                      TRUE ~ total_result
      ),
      lot = case_when(lot == 'X00000' ~ lot_number(uid),
                      TRUE ~ lot)
      ) %>%
      dplyr::select(idi2d, uid, cartridge_SN, sample_id, start_time,
                    AM, OM, ADF_name, ADF_version, total_result, error_code,
                    IC, lot, sample_type, everything())
    
    tests <- bind_rows(tests, test.page)
    
    print(paste0('Page ', 1 + page, ' out of ', n_pages + 1))
  }
  
  return(tests)
}