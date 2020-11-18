#' RCA_batch_regression
#
#' This performs a batch regression using the same inputs as RCA_LOAD_TEST
#'  but with a df
#'
#' @import dplyr
#' @import purrr
#' @import reshape2
#' @import zip
#' 
#' @param df: Dataframe containing: id of the cartridge, output folder, RCA_config, RCA_file, and RCA_conf_mgmt (or RCA, harness, calibration, config, barcode, and ignoreISflag). If a column is named 'expected' the concordance will be computed.
#' @param output: If the output is 'UIA' the output will be an unnested dataframe ready for FUTURO. 'All' to have an output as a nested Dataframe containing amplifications, targets, fails, and messages. 
#' @param cache: Boolean. Should the output be stored?
#' @param filename: Path of the output.
#' 
#' @examples c <- `Regression_ME_FPs` %>%
#'select(testID, 'group' = 'Group', 'expected' = 'Expected.RCA')
#'
#'con <- RMySQL::dbConnect(RMySQL::MySQL(),
#'                         dbname = 'FuturoMilenial',
#'                         user = '*****',
#'                         password = '*****',
#'                         host = 'DB02STAT',
#'                         port = 3306)
#'
#'cartridges_tbl <- tbl(con, 'Cartridges')
#'
#'data <- cartridges_tbl %>% 
#'  select(id, uid) %>% 
#'  filter(uid %in% !!c$testID) %>% 
#'  collect %>% 
#'  mutate(out = "C:\\Users\\jmmerida\\Downloads\\test\\",
#'         csv_folder = "\\\\10.156.1.39\\cartridge_files",
#'         RCA_config = 'rca_men_bundle_2.0.1_NobN',
#'         RCA_file = file.path(file.path('\\\\10.104.48.55', 'RCAExplorer'), 'RCA'),
#'         RCA_conf_mgmt = file.path('RCAConfigurationManagement.csv')) %>% 
#'  left_join(c,
#'            by = c('uid' = 'testID'))
#'
#'RMySQL::dbDisconnect(con)
#'
#'a <- RCA_batch_regression(data,
#'                          cache = TRUE,
#'                          out = 'Theia',
#'                          filename = "C:\\Users\\jmmerida\\Downloads\\test\\test\\test.zip")
#'
#' @name RCA_batch_regression
# By UIA & OE on 27/10/2020

#' @export

RCA_batch_regression <- function(df,
                                 output = 'All',
                                 cache = FALSE,
                                 filename = NULL){
  targets <- NULL
  amplifications <- NULL
  messages <- NULL
  fails <- NULL
  
  i <- 0
  
  if(output == 'Theia'){
    unlink(file.path(tempdir(), 'amplifications'))
    dir.create(file.path(tempdir(), 'amplifications'), showWarnings = FALSE)
  }
  
  if(all(c('RCA_config', 'RCA_file', 'RCA_conf_mgmt') %in% names(df))){
    for(test in df$id){
      i <- i + 1
      tmp <- RCA_load_test(testID = test,
                           out = df$out[df$id == test],
                           csv_folder = df$csv_folder[df$id == test],
                           RCA_config = df$RCA_config[df$id == test],
                           RCA_file = df$RCA_file[df$id == test],
                           RCA_conf_mgmt = df$RCA_conf_mgmt[df$id == test])
      
      if(output == 'All'){
        
        #Concordance
        if(!is.null(df$expected[df$id == test])){
          test_expected <- data.frame(id = test,
                                      expected = df$expected[df$id == test]) %>% 
            separate(expected, 
                     into = c('RC1','RC2','RC3','RC4',
                              'RC5','RC6', 'RC7', 'RC8'),
                     '[/]') %>% 
            melt(id.vars = 'id',
                 variable.name = 'RC',
                 value.name = 'expected') %>% 
            separate(expected,
                     into = c('S0', 'S1', 'S2', 'S3', 'S4', 'S5'),
                     sep = '[+]') %>% 
            melt(id.vars = c('id', 'RC'),
                 variable.name = 'sensor',
                 value.name = 'expected') %>% 
            filter(!is.na(expected)) %>% 
            mutate(sensor = case_when(expected %in% c('D0', 'FAM') ~ 0,
                                      expected %in% c('D1', 'VIC') ~ 1,
                                      expected %in% c('D2', 'NED') ~ 2,
                                      expected %in% c('D3', 'ROX', 'TexasRed') ~ 3,
                                      expected %in% c('D4', 'Cy5', 'TYE') ~ 4,
                                      expected %in% c('D5', 'Cy5.5') ~ 5)) %>% 
            drop_na() %>% 
            mutate(result = case_when(expected != 'Neg' ~ 'Pos',
                                      TRUE ~ 'Neg')) %>% 
            select(-expected) %>%
            mutate_at(vars(RC), ~str_replace_all(., 'RC', '')) %>% 
            mutate_at(vars(RC), as.numeric) %>%
            complete(nesting(id),
                     RC = seq(1, 8, by = 1),
                     fill = list(result = 'Neg')) %>% 
            complete(nesting(id, RC),
                     sensor = seq(0, 5, by = 1),
                     fill = list(result = 'Neg')) %>% 
            drop_na() %>% 
            rename('expected' = 'result') %>% 
            mutate_at(vars(RC, sensor), as.character)
        }
        
        if(!is.null(df$expected[df$id == test]) &
           !is.null(tmp$targets)){
          
          tmp$targets <- tmp$targets %>% 
            left_join(test_expected,
                      by = c('testID' = 'id',
                             'rc' = 'RC',
                             'ch' = 'sensor')) %>% 
            mutate(concordance = case_when(
              result == 'Pos' & expected == 'Pos' ~ 'TP',
              result == 'Pos' & expected == 'Neg' ~ 'FP',
              result == 'Neg' & expected == 'Pos' ~ 'FN',
              result == 'Neg' & expected == 'Neg' ~ 'TN',
              TRUE ~ 'Unknown'))
          }
        
        targets <- bind_rows(targets, tmp$targets)
        amplifications <- bind_rows(amplifications, tmp$amplifications)
        messages <- bind_rows(messages, tmp$messages)
        fails <- bind_rows(fails, as.data.frame(tmp$fail) %>%
                             mutate(testID = test) %>% 
                             mutate_at(vars(matches('cp')), as.numeric)) 
  
      }
      else if(output == 'Theia'){
        
        #Concordance
        if(!is.null(df$expected[df$id == test])){
          test_expected <- data.frame(id = test,
                                      expected = df$expected[df$id == test]) %>% 
            separate(expected, 
                     into = c('RC1','RC2','RC3','RC4',
                              'RC5','RC6', 'RC7', 'RC8'),
                     '[/]') %>% 
            melt(id.vars = 'id',
                 variable.name = 'RC',
                 value.name = 'expected') %>% 
            separate(expected,
                     into = c('S0', 'S1', 'S2', 'S3', 'S4', 'S5'),
                     sep = '[+]') %>% 
            melt(id.vars = c('id', 'RC'),
                 variable.name = 'sensor',
                 value.name = 'expected') %>% 
            filter(!is.na(expected)) %>% 
            mutate(sensor = case_when(expected %in% c('D0', 'FAM') ~ 0,
                                      expected %in% c('D1', 'VIC') ~ 1,
                                      expected %in% c('D2', 'NED') ~ 2,
                                      expected %in% c('D3', 'ROX', 'TexasRed') ~ 3,
                                      expected %in% c('D4', 'Cy5', 'TYE') ~ 4,
                                      expected %in% c('D5', 'Cy5.5') ~ 5)) %>% 
            drop_na() %>% 
            mutate(result = case_when(expected != 'Neg' ~ 'Pos',
                                      TRUE ~ 'Neg')) %>% 
            select(-expected) %>%
            mutate_at(vars(RC), ~str_replace_all(., 'RC', '')) %>% 
            mutate_at(vars(RC), as.numeric) %>%
            complete(nesting(id),
                     RC = seq(1, 8, by = 1),
                     fill = list(result = 'Neg')) %>% 
            complete(nesting(id, RC),
                     sensor = seq(0, 5, by = 1),
                     fill = list(result = 'Neg')) %>% 
            drop_na() %>% 
            rename('expected' = 'result') %>% 
            mutate_at(vars(RC, sensor), as.character)
        }
        
        if(!is.null(df$expected[df$id == test]) &
           !is.null(tmp$targets)){
          
          tmp$targets <- tmp$targets %>% 
            left_join(test_expected,
                      by = c('testID' = 'id',
                             'rc' = 'RC',
                             'ch' = 'sensor')) %>% 
            mutate(concordance = case_when(
              result == 'Pos' & expected == 'Pos' ~ 'TP',
              result == 'Pos' & expected == 'Neg' ~ 'FP',
              result == 'Neg' & expected == 'Pos' ~ 'FN',
              result == 'Neg' & expected == 'Neg' ~ 'TN',
              TRUE ~ 'Unknown'))
        }
        tmp$targets <- tmp$targets %>% 
          {if(!is.null(.)){
            select(.,
                   testID, rc, ch, ct, ep, ep_cp7,
                   baseline_pre, cp8_dEP, cp8_snr, cp8_rmse, cp8_ct,
                   alpha, beta, a, b, c0, result,
                   expected, concordance) %>% 
              mutate_at(vars(rc, ch), as.numeric) %>% 
              mutate(idx = paste0(testID, rc, ch)) 
          } else{
            .
          }
          }
        
        
        targets <- bind_rows(targets, tmp$targets)
        
        tmp$amplifications <- tmp$amplifications %>% 
          {if(!is.null(.)){
            mutate_at(.,
                      vars(rc, ch),
                      as.numeric) %>% 
              mutate(idx = paste0(testID, rc, ch))
          } else{
            .
          }}
        
        for(rcch in unique(tmp$amplifications$idx)){
          saveRDS(tmp$amplifications %>% 
                    filter(idx == rcch),
                  file = file.path(tempdir(),
                                   'amplifications',
                                   paste0(rcch, '.RDS')))
        }
        
        
        messages <- bind_rows(messages,
                              tmp$messages)
        
        fails <- bind_rows(fails, as.data.frame(tmp$fail) %>%
                             mutate(testID = test) %>% 
                             mutate_at(vars(matches('cp')), as.numeric))
        
        
      }  
      else{
        
        fails <- bind_rows(fails, as.data.frame(tmp$fail) %>%
                             mutate(testID = test) %>% 
                             mutate_at(vars(matches('cp')), as.numeric)) 
      
      }
      
      cat(test, ' Processed\t', i, ' out of ', length(df$id),'\n')
      
    }
  } else{
    for(test in df$id){
      i <- i+1
      tmp <- RCA_load_test(testID = test,
                           out = df$out[df$id == test],
                           csv_folder = df$csv_folder[df$id == test],
                           RCA_config = df$RCA_config[df$id == test],
                           RCA = df$RCA[df$id == test],
                           harness = df$harness[df$id == test],
                           calibration = df$calibration[df$id == test],
                           config = df$config[df$id == test],
                           barcode = df$barcode[df$id == test])
      
      if(output == 'All'){
        
        #Concordance
        if(!is.null(df$expected[df$id == test]) &
           !is.na(df$expected[df$id == test])){
          test_expected <- data.frame(id = test,
                                      expected = df$expected[df$id == test]) %>% 
            separate(expected, 
                     into = c('RC1','RC2','RC3','RC4',
                              'RC5','RC6', 'RC7', 'RC8'),
                     '[/]') %>% 
            melt(id.vars = 'id',
                 variable.name = 'RC',
                 value.name = 'expected') %>% 
            separate(expected,
                     into = c('S0', 'S1', 'S2', 'S3', 'S4', 'S5'),
                     sep = '[+]') %>% 
            melt(id.vars = c('id', 'RC'),
                 variable.name = 'sensor',
                 value.name = 'expected') %>% 
            filter(!is.na(expected)) %>% 
            mutate(sensor = case_when(expected %in% c('D0', 'FAM') ~ 0,
                                      expected %in% c('D1', 'VIC') ~ 1,
                                      expected %in% c('D2', 'NED') ~ 2,
                                      expected %in% c('D3', 'ROX') ~ 3,
                                      expected %in% c('D4', 'Cy5') ~ 4,
                                      expected %in% c('D5', 'Cy5.5') ~ 5)) %>% 
            drop_na() %>% 
            mutate(result = case_when(expected != 'Neg' ~ 'Pos',
                                      TRUE ~ 'Neg')) %>% 
            select(-expected) %>%
            mutate_at(vars(RC), ~str_replace_all(., 'RC', '')) %>% 
            mutate_at(vars(RC), as.numeric) %>%
            complete(nesting(id),
                     RC = seq(1, 8, by = 1),
                     fill = list(result = 'Neg')) %>% 
            complete(nesting(id, RC),
                     sensor = seq(0, 5, by = 1),
                     fill = list(result = 'Neg')) %>% 
            drop_na() %>% 
            rename('expected' = 'result') %>% 
            mutate_at(vars(RC, sensor), as.character)
        }
        
        if(!is.null(df$expected[df$id == test]) &
           !is.na(df$expected[df$id == test]) &
           !is.null(tmp$targets)){
          
          tmp$targets <- tmp$targets %>% 
            left_join(test_expected,
                      by = c('testID' = 'id',
                             'rc' = 'RC',
                             'ch' = 'sensor')) %>% 
            mutate(concordance = case_when(
              result == 'Pos' & expected == 'Pos' ~ 'TP',
              result == 'Pos' & expected == 'Neg' ~ 'FP',
              result == 'Neg' & expected == 'Pos' ~ 'FN',
              result == 'Neg' & expected == 'Neg' ~ 'TN',
              TRUE ~ 'Unknown'))
        }
        
        targets <- bind_rows(targets, tmp$targets)
        amplifications <- bind_rows(amplifications, tmp$amplifications)
        messages <- bind_rows(messages, tmp$messages)
        fails <- bind_rows(fails, as.data.frame(tmp$fail) %>%
                             mutate(testID = test) %>% 
                             mutate_at(vars(matches('cp')), as.numeric)) 
          
      }
      else if(output == 'Theia'){
        
        #Concordance
        if(!is.null(df$expected[df$id == test])){
          test_expected <- data.frame(id = test,
                                      expected = df$expected[df$id == test]) %>% 
            separate(expected, 
                     into = c('RC1','RC2','RC3','RC4',
                              'RC5','RC6', 'RC7', 'RC8'),
                     '[/]') %>% 
            melt(id.vars = 'id',
                 variable.name = 'RC',
                 value.name = 'expected') %>% 
            separate(expected,
                     into = c('S0', 'S1', 'S2', 'S3', 'S4', 'S5'),
                     sep = '[+]') %>% 
            melt(id.vars = c('id', 'RC'),
                 variable.name = 'sensor',
                 value.name = 'expected') %>% 
            filter(!is.na(expected)) %>% 
            mutate(sensor = case_when(expected %in% c('D0', 'FAM') ~ 0,
                                      expected %in% c('D1', 'VIC') ~ 1,
                                      expected %in% c('D2', 'NED') ~ 2,
                                      expected %in% c('D3', 'ROX', 'TexasRed') ~ 3,
                                      expected %in% c('D4', 'Cy5', 'TYE') ~ 4,
                                      expected %in% c('D5', 'Cy5.5') ~ 5)) %>% 
            drop_na() %>% 
            mutate(result = case_when(expected != 'Neg' ~ 'Pos',
                                      TRUE ~ 'Neg')) %>% 
            select(-expected) %>%
            mutate_at(vars(RC), ~str_replace_all(., 'RC', '')) %>% 
            mutate_at(vars(RC), as.numeric) %>%
            complete(nesting(id),
                     RC = seq(1, 8, by = 1),
                     fill = list(result = 'Neg')) %>% 
            complete(nesting(id, RC),
                     sensor = seq(0, 5, by = 1),
                     fill = list(result = 'Neg')) %>% 
            drop_na() %>% 
            rename('expected' = 'result') %>% 
            mutate_at(vars(RC, sensor), as.character)
        }
        
        if(!is.null(df$expected[df$id == test]) &
           !is.null(tmp$targets)){
          
          tmp$targets <- tmp$targets %>% 
            left_join(test_expected,
                      by = c('testID' = 'id',
                             'rc' = 'RC',
                             'ch' = 'sensor')) %>% 
            mutate(concordance = case_when(
              result == 'Pos' & expected == 'Pos' ~ 'TP',
              result == 'Pos' & expected == 'Neg' ~ 'FP',
              result == 'Neg' & expected == 'Pos' ~ 'FN',
              result == 'Neg' & expected == 'Neg' ~ 'TN',
              TRUE ~ 'Unknown'))
        }
        tmp$targets <- tmp$targets %>% 
          {if(!is.null(.)){
            select(.,
                   testID, rc, ch, ct, ep, ep_cp7,
                   baseline_pre, cp8_dEP, cp8_snr, cp8_rmse, cp8_ct,
                   alpha, beta, a, b, c0, result,
                   expected, concordance) %>% 
              mutate_at(vars(rc, ch), as.numeric) %>% 
              mutate(idx = paste0(testID, rc, ch)) 
          } else{
            .
          }
          }
        
        
        targets <- bind_rows(targets, tmp$targets)
        
        tmp$amplifications <- tmp$amplifications %>% 
          {if(!is.null(.)){
            mutate_at(.,
                      vars(rc, ch),
                      as.numeric) %>% 
              mutate(idx = paste0(testID, rc, ch))
          } else{
            .
          }}
        
        for(rcch in unique(tmp$amplifications$idx)){
          saveRDS(tmp$amplifications %>% 
                    filter(idx == rcch),
                  file = file.path(tempdir(),
                                   'amplifications',
                                   paste0(rcch, '.RDS')))
        }
        
        
        messages <- bind_rows(messages,
                              tmp$messages)
        
        fails <- bind_rows(fails, as.data.frame(tmp$fail) %>%
                             mutate(testID = test) %>% 
                             mutate_at(vars(matches('cp')), as.numeric))
        
        
      }  
      else{
        fails <- bind_rows(fails, as.data.frame(tmp$fail) %>%
                             mutate(testID = test) %>% 
                             mutate_at(vars(matches('cp')), as.numeric))
        }
      cat(test, ' Processed\t', i, ' out of ', length(df$id),'\n')
    }
  }
  
  #Build output
  df <- df %>% 
    nest(metadata = -id) %>% 
    {if(!is.null(targets)){
      left_join(.,
                targets %>%  nest(targets = -testID),
                by = c('id' = 'testID'))
    } else{
      .
    }} %>% 
    {if(!is.null(amplifications)){
      left_join(.,
                amplifications %>%  nest(amplifications = -testID),
                by = c('id' = 'testID'))
    } else{
      .
    }} %>% 
    {if(!is.null(messages)){
      left_join(.,
                messages %>%  nest(messages = -testID),
                by = c('id' = 'testID'))
    } else{
      .
    }} %>% 
    {if(!is.null(fails)){
      left_join(.,
                fails %>%  nest(fails = -testID),
                by = c('id' = 'testID'))
    } else{
      .
    }}
  
  if(output == 'UIA'){
    df <- df %>% 
      select(id, fails) %>% 
      unnest(fails)
  }
  
  if(cache){
    save(df, file = filename)
  }
  
  if(cache & output == 'Theia'){
    saveRDS(df, file = file.path(tempdir(), 'amplifications', 'df.RDS'))
    print(tempdir())
    zip::zip(filename,
             normalizePath(list.files(file.path(tempdir(), 'amplifications'),
                                      full.names = TRUE),
                           winslash = '\\'),
             mode = 'cherry-pick')
    
    print(normalizePath(list.files(file.path(tempdir(), 'amplifications'),
                                   full.names = TRUE)))
  }
  
  return(df)
}
