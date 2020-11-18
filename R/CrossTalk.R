#' CrossTalk
#
#' Performs a CrossTalk analysis.
#' 
#' @param cartridge_list: dataframe containing the id and the RCA calls in an column called call. In long format.
#' @param output_path: file.path where the csv are located. (id.csv)
#' @param ch0: Integration time for Sensor 0.
#' @param ch1: Integration time for Sensor 1.
#' @param ch2: Integration time for Sensor 2.
#' @param ch3: Integration time for Sensor 3.
#' @param ch4: Integration time for Sensor 4.
#' @param ch5: Integration time for Sensor 5.
#' @param All: Boolean. Should all co-amplifications be computed?
#' @param measure: Select measure (desc. order)
#'
#' @import dplyr
#' @import purrr
#' @import XML
#' @import stringr
#' @import reshape2
#' 
# By UIA & OE on 14/05/2020

#' @export

CrossTalk <- function(cartridge_list,
                      output_path,
                      ch0 = 10,
                      ch1 = 20,
                      ch2 = 20,
                      ch3 = 10,
                      ch4 = 10,
                      ch5 = 20,
                      All = FALSE,
                      measure = 1){
  
  id_to_use <- cartridge_list %>% 
    distinct(id)
  
  xtalk <- NULL
  
  i = 0
  for(id in id_to_use$id){
    
    i = i+1
    
    filename <- file.path(output_path, paste0(id, '.csv'))
    
    if(file.exists(filename) & file.size(filename) > 5000){
      raw_data <- read.csv(filename,
                           sep = '\t') %>%
        group_by(Cycle, RC) %>% 
        arrange(desc(S0), desc(S1), desc(S2),
                desc(S3), desc(S4), desc(S5)) %>% 
        mutate(measure_id = row_number()) %>% 
        filter(measure_id == measure) %>% 
        ungroup() %>% 
        select(Cycle, RC, matches('S[0-5]')) %>%
        filter(Cycle > 0) %>%
        melt(id.vars = c('Cycle', 'RC'),
             variable.name = 'Sensor',
             value.name = 'fluorescence'
        ) %>% 
        rename('rc' = 'RC') %>% 
        mutate_at(vars(Sensor), ~str_remove(., 'S')) %>% 
        mutate_at(vars(Sensor), as.numeric) %>% 
        filter(fluorescence < 1e6) %>% 
        mutate(fluorescence = case_when(
          Sensor == 0 ~ fluorescence * 20 / ch0,
          Sensor == 1 ~ fluorescence * 20 / ch1,
          Sensor == 2 ~ fluorescence * 20 / ch2,
          Sensor == 3 ~ fluorescence * 20 / ch3,
          Sensor == 4 ~ fluorescence * 20 / ch4,
          Sensor == 5 ~ fluorescence * 20 / ch5))
      
      cartridge_xtalk <- cartridge_list %>% 
        filter(id == !!id,
               call == '$positive') %>% 
        left_join(raw_data, by = c('rc' = 'rc',
                                   'ch' = 'Sensor')) %>% 
        left_join(raw_data, by = c('rc' = 'rc',
                                   'Cycle' = 'Cycle')) %>% 
        filter(Cycle > round(ct, 0) - 1) %>% 
        mutate(chfl = paste0(ch, Sensor)) %>% 
        mutate(rcch_1 = paste0(rc, ch),
               rcch_2 = paste0(rc, Sensor)) %>% 
        mutate_at(vars(rcch_1, rcch_2), as.numeric) %>% 
        left_join(cartridge_list %>%
                    mutate(rcch = as.numeric(paste0(rc, ch))) %>% 
                    select(id, rcch, call),
                  by = c('id' = 'id',
                         'rcch_2' = 'rcch')) %>% 
        rowwise() %>% 
        {if(!All){
          filter(.,
                 rcch_2 %in% c(rcch_1 + 1, rcch_1 - 1),
                 (call.y != '$positive' | is.na(call.y)))
        } else{
          filter(.,
                 rcch_2 %in% c(rcch_1 + 1, rcch_1 - 1))
        }} %>% 
        select(-contains('call')) %>% 
        group_by(
          id, rc, ch, Sensor, chfl, rcch_1
        ) %>%
        {if (nrow(.) > 0){
          summarise(.,
                    fit.params = fit_params(fluorescence.x, fluorescence.y)) %>%
            separate(
              col = fit.params,
              into = c('intercept', 'slope', 'R2'),
              sep = ';')
        }}
      
      xtalk <- bind_rows(xtalk, cartridge_xtalk)
      
      cat(id, 'Processed\t', i, ' out of ', length(id_to_use$id),'\n')
    } else{
      
      cat(id, 'Does not exist\t', i, ' out of ', length(id_to_use$id),'\n')
    }
    
  }
  
  return(xtalk)
}

fit_params <- function(x, y) {
  # Fits a linear model that relates the cross-talk in channel "y" produced by
  # channel "x".
  # The function returns "intercept", "slope", and "R^2" seppareted by ";".
  lm_tmp <- lm(y ~ x)
  return(paste0(c(
    lm_tmp$coefficients,
    summary(lm_tmp)$r.squared
  ),
  collapse = ";"
  ))
}