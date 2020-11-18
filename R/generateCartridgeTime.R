#' generateCartridgeTime
#
#' This function returns the metadata for a set of cartridges based on the
#' date.
#'
#' @import httr
#' @import dplyr
# By Lucía & José on 26/02/2020
#


#' @export
generateCartridgeTime <- function(fromTime = 0,
                                 in2data.url =
                              "https://in2data.qiagen.com/in2data/backend/api/"
){
  if(class(fromTime) == 'character'){
    fromTime <- format(as.POSIXct(fromTime), '%Y-%m-%dT%H:%M:%S.000Z') 
  }
  
  Token <- generateToken()
  
  results.page <- POST(paste0(in2data.url, "results/0/50"),
                       timeout(300),
                       add_headers("Content-Type"="application/json",
                                   "Authorization"= Token),
                       body = paste0('{"sorting":[{"column":"created","direction":"asc","sortAlias":"created"}],', 
                                     '"filters":{',
                                     '"created":{"value":{"from":"',
                                     fromTime, '"',
                                     '},"operation":"CONTAINS"}',
                                     '}}'))
  
  calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 1
  
  while(results.page$status_code == 429){
    Sys.sleep(0.5)
    results.page <- POST(paste0(in2data.url, "results/0/50"),
                         timeout(300),
                         add_headers("Content-Type"="application/json",
                                     "Authorization"= Token),
                         body = paste0('{"sorting":[{"column":"created","direction":"asc","sortAlias":"created"}],', 
                                       '"filters":{',
                                       '"created":{"value":{"from":"',
                                       fromTime, '"',
                                       '},"operation":"CONTAINS"}',
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
                         timeout(300),
                         add_headers("Content-Type"="application/json",
                                     "Authorization"= Token),
                         body = paste0('{"sorting":[{"column":"created","direction":"asc","sortAlias":"created"}],', 
                                       '"filters":{',
                                       '"created":{"value":{"from":"',
                                       fromTime, '"',
                                       '},"operation":"CONTAINS"}',
                                       '}}'))
    
    Token <- unname(results.page$request$headers["Authorization"])
    
    calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 1
    
    while(results.page$status_code == 429){
      Sys.sleep(0.5)
      results.page <- POST(paste0(in2data.url, paste0('results/',
                                                      page,
                                                      '/50')),
                           timeout(300),
                           add_headers("Content-Type"="application/json",
                                       "Authorization"= Token),
                           body = paste0('{"sorting":[{"column":"created","direction":"asc","sortAlias":"created"}],', 
                                         '"filters":{',
                                         '"created":{"value":{"from":"',
                                         fromTime, '"',
                                         '},"operation":"CONTAINS"}',
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
      bind_rows(data.frame(cartridgeSerialNumber = character(0),
                           sampleIdentifier = character(0),
                           amName = character(0),
                           omName = character(0),
                           assayName = character(0),
                           assayVersion = character(0),
                           errorCode = character(0),
                           controlsPassed = character(0),
                           amVersion = character(0),
                           omVersion = character(0),
                           barcode = character(0),
                           patientIdentifier = character(0),
                           totalResult = character(0),
                           created = character(0))) %>% 
      mutate_all(as.character) %>%
      mutate(uid = case_when(cartridgeSerialNumber == 'P00000007' ~
                               sampleIdentifier,
                             TRUE ~  cartridgeSerialNumber)
      ) %>%
      rename('cartridge_SN' = 'cartridgeSerialNumber',
             'sample_id' = 'sampleIdentifier',
             'start_time' = 'startTimestamp',
             'AM' = 'amName',
             'OM' = 'omName',
             'ADF_name' = 'assayName',
             'ADF_version' = 'assayVersion',
             'error_code' = 'errorCode',
             'IC' = 'controlsPassed',
             'AM_firmware' = 'amVersion',
             'OM_version' = 'omVersion',
             'barcode' = 'barcode',
             'patient_ID' = 'patientIdentifier',
             'total_result' = 'totalResult',
             'sample_type' = 'sampleType.name',
             'created_in2data' = 'created') %>%
      mutate(total_result = case_when(total_result == 'POSITIVE' ~
                                        'pos',
                                      total_result == 'POSITIVE_WITH_WARNINGS' ~
                                        'pos*',
                                      total_result == 'NEGATIVE' ~
                                        'neg',
                                      total_result == 'FAILED' ~
                                        'fail',
                                      total_result == 'NOT_PRESENT' ~
                                        'NOT_PRESENT')
      ) %>% 
      select(uid, id, cartridge_SN, lotNumber, sample_id, start_time, AM, OM,
             ADF_name, ADF_version, total_result, error_code, IC, AM_firmware,
             OM_version, barcode, patient_ID, sample_type, contains("result"),
             contains("ct"), contains("ep"),
             contains('name'), -contains('short'),
             contains('baseline'), created_in2data, location, site,
             -resultIdentifier) %>%
      melt(id.vars = 'id') %>%
      mutate_at(vars(variable), ~str_replace_all(.,
                                                 c('chambers[.]' = '',
                                                   'controls[.]' = '',
                                                   'nonTargets[.]' = '',
                                                   'RC([1-9])S([0-9])' = '\\1\\2',
                                                   '[.]result' = 'call_',
                                                   '([0-9]+)(call_)' = '\\2\\1',
                                                   '[.]ct' = 'ct_',
                                                   '([0-9]+)(ct_)' = '\\2\\1',
                                                   '[.]ep' = 'dep_',
                                                   '([0-9]+)(dep_)' = '\\2\\1',
                                                   '[.]label' = 'labels_',
                                                   '([0-9]+)(labels_)' = '\\2\\1',
                                                   '([0-9]+)[.](name)' = '\\2_\\1',
                                                   '[.]baseline' = 'baseline_',
                                                   '([0-9]+)(baseline_)' = '\\2\\1'))) %>%
      drop_na() %>%
      distinct(id, variable, .keep_all = TRUE) %>%
      dcast(id ~ variable) %>%
      mutate_at(vars(contains('ep')), ~str_replace_all(., ',', '')) %>%
      mutate_at(vars(contains('ct'),
                     contains('dep'),
                     contains('baseline'),
                     AM,
                     OM,
                     id,
                     start_time,
                     created_in2data),
                as.numeric) %>%
      select(id, uid, everything())
    
    tests <- bind_rows(tests, test.page)
    
    print(paste0('Page ', 1 + page, ' out of ', n_pages + 1))
  }
  
  return(tests)
}