# generateCartridge
#
# This function returns the metadata for an OM. OM id should be provided
#
#' @import httr
#' @import dplyr
# By Lucía & José on 26/02/2020
#

#' @export
generateCartridge <- function(list,
                              in2data.url =
                              "https://in2data.qiagen.com/in2data/backend/api/",
                              fromTime = '1980-01-01'
){
  
  fromTime <- format(as.Date(fromTime), '%Y-%m-%dT%H:%M:%S.000Z')
  
  Token <- generateToken()

  results.page <- POST(paste0(in2data.url, "results/0/50"),
                       timeout(30),
                       add_headers("Content-Type"="application/json",
                                   "Authorization"= Token),
                       body = paste0('{"filters":{"cartridgeSerialNumber":{"value":"',
                                     paste(list$testID, collapse = '\\n'),
                                     '","operation":"CONTAINS"},',
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
                         body = paste0('{"filters":{"cartridgeSerialNumber":{"value":"',
                                       paste(list$testID, collapse = '\\n'),
                                       '","operation":"CONTAINS"},',
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
                         body = paste0('{"filters":{"cartridgeSerialNumber":{"value":"',
                                       paste(list$testID, collapse = '\\n'),
                                       '","operation":"CONTAINS"},',
                                       '"startTimestamp":{"value":{"from":"',
                                       fromTime, '"',
                                       '},"operation":"EQ"}',
                                       '}}'))
    # print(body)
    # print(results.page$status_code)
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
                           body = paste0('{"filters":{"cartridgeSerialNumber":{"value":"',
                                         paste(list$testID, collapse = '\\n'),
                                         '","operation":"CONTAINS"},',
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
      rename(cartridge_SN = cartridgeSerialNumber,
             sample_id = sampleIdentifier,
             start_time = startTimestamp,
             AM = amName,
             OM = omName,
             ADF_name = assayName,
             ADF_version = assayVersion,
             error_code = errorCode,
             IC = controlsPassed,
             AM_firmware = amVersion,
             OM_version = omVersion,
             Barcode = barcode,
             Patient_ID = patientIdentifier,
             total_result = totalResult,
             sample_type = 'sampleType.name') %>%
      mutate(total_result = case_when(total_result == 'POSITIVE' ~
                                        'pos',
                                      total_result == 'POSITIVE_WITH_WARNINGS' ~
                                        'pos*',
                                      total_result == 'NEGATIVE' ~
                                        'neg',
                                      total_result == 'FAILED' ~
                                        'fail')
      ) %>%
      select(uid, id, cartridge_SN, sample_id, start_time, AM, OM, ADF_name,
             ADF_version, total_result, error_code, IC, AM_firmware,
             OM_version, Barcode, Patient_ID, sample_type, contains("result"),
             contains("ct"), contains("ep"), contains("label"),
             everything())

    tests <- bind_rows(tests, test.page)
    print(paste0('Page ', 1 + page, ' out of ', n_pages + 1))
  }

  return(tests)
}
