#' RCA.load.test
#
#' This performs a regression using the following inputs:
#' @param testID: SN of the cartridge to execute
#' @param out: path of the output folder
#' @param RCA_config: chr. Bundle to use
#' @param csv_folder: path of the \*.csv files
#' @param RCA_file : path of the RCA java file
#' @param RCA_conf_mgmt: LUT of RCA bundles
#' @param RCA: RCA jar file
#' @param harness: harness jar file
#' @param calibration: calibration csv
#' @param config: config xml
#' @param barcode: carcode txt
#'
#' @import dplyr
#' @import purrr
#' @import XML
#' @import stringr
#' @import reshape2
#' 
#' @name RCA_load_test
#' @export

RCA_load_test <- function(testID,
                          out,
                          csv_folder,
                          RCA_config,
                          RCA_file = NULL,
                          RCA_conf_mgmt = NULL,
                          RCA = NULL,
                          harness = NULL,
                          calibration = NULL,
                          config = NULL,
                          barcode = NULL,
                          ignoreISflag = ' -i') {
  
  path_XML_RCA_log <- file.path(out,  RCA_config)
  file_R_RCA_log <- file.path(path_XML_RCA_log,  paste0(testID, ".rds"))
  
  da <- NULL
  
  if(file.exists(file_R_RCA_log)){
    return(readRDS(file_R_RCA_log))
  } else {
    # load xml 
    log <- RCA.XMLlog.get(RCA_config,
                          testID,
                          out,
                          csv_folder,
                          RCA_file,
                          RCA_conf_mgmt,
                          RCA,
                          harness,
                          calibration,
                          config,
                          barcode,
                          ignoreISflag)
    
    if (!is.null(log)){
      
      doc <- xmlParse(log)
      if(!is.character(doc) & !is.data.frame(doc)){
        # Get messages 
        tmp <- try(bind_rows(xmlSApply(getNodeSet(doc, "//msg"),
                                   xmlAttrs)),
                   silent = TRUE)
        if(class(tmp) == 'try-error'){
          tmp <- as.data.frame(t(xmlSApply(getNodeSet(doc, "//msg"),
                                           xmlAttrs)))
        }
          tmp <- tmp %>% 
            select(-verbosity, -class) %>% 
            mutate(testID = testID,
                   cp = as.numeric(cp),
                   result = factor(result))
          
        a <- xmlSApply(getNodeSet(doc, "//msg"),
                       xmlAttrs)
        da$messages <- tmp
        
        # Get barcode header data 
        tmp <- xmlSApply(getNodeSet(doc, '//test'), xmlAttrs)
        
        da$barcode.header <- tmp
        
        # Fail detector 
        fail <- da$messages %>% 
          when(nrow(filter(., result == 'NOT_ENOUGH_DATA')) > 0 ~ 
                 filter(., result == 'NOT_ENOUGH_DATA') %>% slice(1),
               nrow(filter(., result == 'FAILED')) > 0 ~ 
                  filter(., result == 'FAILED'),
               !any(grepl('[(]positive result found[)]', .$text)) ~
                 'No amplifications')
        
        # Get targets
        
        if (is.null(fail)){
          tmp <- RCA.targets.get(doc) %>% 
            mutate(testID = testID)
          da$fail <- NULL
          targets <- tmp
          da$test <- data.frame(testID = testID,
                                result = gsub("('|\\$)",
                                              "",
                                              xmlToDataFrame(getNodeSet(doc, "//results_table//overall"),
                                                             stringsAsFactors = FALSE)[1]))
          
          # recover Baseline info for targets table
          tmp <- subset(da$messages,
                        cp==12 & codeblock == "Detailed Baseline Finding") %>% 
            mutate(baseline = as.numeric(
              gsub("(^.*intercept[[:blank:]]*)(-*[[:digit:]]*[.][[:digit:]].)(.*$)",
                   "\\2",
                   tmp$text))) %>% 
            select('testID', 'rc', 'ch', 'baseline')
          
          targets <- merge(targets,
                           tmp,
                           by = c("testID", "rc", "ch"),
                           all.x = TRUE)
          
          #recover Logistic fit info for targets table
          tmp <- subset(da$messages,
                        cp==8 & codeblock == "Parametric Curve Fitting") %>% 
            mutate(alpha = as.numeric(
              gsub('(^.* alpha [=][[:blank:]]*)(-*[[:digit:]]*[.][[:digit:]].)(.*$)',
                   '\\2',
                   text)),
              beta = as.numeric(
                gsub("(^.* beta [=][[:blank:]]*)(-*[[:digit:]]*[.][[:digit:]].)(.*$)",
                     "\\2",
                     text)),
              a = as.numeric(
                gsub("(^.* a [=][[:blank:]]*)(-*[[:digit:]]*[.][[:digit:]].)(.*$)",
                     "\\2",
                     text)),
              b = as.numeric(
                gsub("(^.* b [=][[:blank:]]*)(-*[[:digit:]]*[.][[:digit:]].)(.*$)",
                     "\\2",
                     text)),
              c0 = as.numeric(
                gsub("(^.* c0 [=][[:blank:]]*)(-*[[:digit:]]*[.][[:digit:]].)(.*$)",
                     "\\2",
                     text))) %>% 
            select("testID", "rc", "ch", "alpha", "beta", "a", "b", "c0")
          
          targets <- merge(targets,
                           tmp,
                           by = c("testID", "rc", "ch"),
                           all.x = TRUE)
          
          da$targets <- targets
          
          
        } else {
          da$fail <- fail
          da$targets <- NULL
          da$test <- data.frame(testID = testID, result ='fail')
          da$amplifications <- NULL
        } 
        
        if(is.null(fail)){
          # Get amplification plots
          tmp <- RCA.amplifications.get(doc) %>% 
            mutate(testID = testID)
          
          da$amplifications <- tmp
        }else{
          tmp <- try(RCA.amplifications.get(doc), silent = TRUE) %>% 
            {if(class(.) == 'try-error'){
              .
            }else{
              mutate(.,
                     testID = testID)
            }}
            
          
          da$amplifications <- tmp
        }
        
      }else{
        da$fail <- doc
        da$targets <- NULL
        da$test <- data.frame(testID = testID, result ='fail')
        da$amplifications <- NULL
      }
      
      #create dir if it does not exists
      if (!file.exists(path_XML_RCA_log)){
        dir.create(path_XML_RCA_log)
      } 
      
      saveRDS(da, file = file_R_RCA_log)
      
      return(da)
      
    } else {return(da)}
  }
}

xmlParse <- function(xml){
  out <- tryCatch(
    {
      xmlTreeParse(xml,
                   useInternalNodes = TRUE)
    },
    error = function(cond){
      problem <- as.data.frame(xml) %>% 
        filter_all(~grepl('fail', ., ignore.case = TRUE))
      problem <- as.character(problem[1, 1])
      problem <- data.frame(garbage = unlist(
        str_extract_all(problem, "[a-zA-Z]+='[ .,=0-9a-zA-Z]+'"))) %>% 
        separate(garbage, c('tag', 'value'), sep = '=', extra = 'merge') %>%
        as.data.frame(row.names = .$tag) %>%
        select(-tag)
      if (all(is.na(problem))){
        return('Not enough data. Cannot coerce')
      } else{
        problem <- as.data.frame(t(problem)) %>% 
          mutate_all(~str_replace_all(.,"'", '')) %>% 
          select(any_of(c('cp', 'codeblock', 'rc', 'ch', 'result', 'text')))
        return(problem)
      }
    }
  )
  return(out)
} 

RCA.config.get <- function(RCA_cfg,
                           path_exec,
                           config){
  # Returns the parameters for a given bundle [Reconsider structure]
  read.csv(file.path(path_exec, config),
           sep = ';',
           encoding = 'UTF-8') %>% 
    filter(CFG == RCA_cfg)
}

RCA.csv.to.XMLlog <- function(testID,
                              RCA_config,
                              csv_folder,
                              RCA_file,
                              RCA_conf_mgmt,
                              RCA,
                              harness,
                              calibration,
                              config,
                              barcode,
                              ignoreISflag) {
  # Convert csv to XML processed by a given RCA bundle
  log <- tryCatch(
    {
      if (file.exists(file.path(csv_folder, paste0(testID, '.csv')))){ 
        # check if file exists
        # Retrieve RCA configuration
        if(!any(c(is.null(RCA_config), is.null(RCA_file), is.null(RCA_conf_mgmt)))){
          RCA_cfg <- RCA.config.get(RCA_config, RCA_file, RCA_conf_mgmt)
          # configure RCA test harness
          harness <- RCA_cfg$Harness
          RCA <- file.path(RCA_file, RCA_cfg$RCA)
          calibration <- file.path(RCA_file, RCA_cfg$HW)
          config <-  file.path(RCA_file, RCA_cfg$RCAConfig)
          barcode <-  file.path(RCA_file, RCA_cfg$Barcode)
          ignoreISflag <- ifelse(RCA_cfg$ignoreIS == 1, ' -i ', '')
        }
        
        #Execute Java RCA through harness
        
        # Remove "cmd.exe /c" in linux
        
        command <- paste0('cmd.exe /c java -Duser.language=EN -jar ',
                          RCA_file,
                          harness,
                          ' -l FINE  -t ',
                          RCA, ' ',
                          calibration, ' ',
                          config, ' ',
                          barcode, ' ', 
                          file.path(csv_folder, paste0(testID,  '.csv')),
                          ' -s ', testID, ignoreISflag, ' 2>&1')
        print(command)
        log <- system(command, intern = TRUE)
        
        # Remove log garbage
        log <- sub('^[^<]*((FINE(R|))|WARNING|INFO):*[[:blank:]]*((?=<)|(?=[^<]).*)',
                   '',
                   log,
                   perl = TRUE)
        log <- log[log != '']
        #  Get only what is enclosde <test> tag
        log <- log[which(grepl('<RCA.*>.*', log)) : which(grepl('</RCA>.*', log))]
        # Remove lines that contains no XML tags
        log <- log[grepl("^.*<.*>.*",log)]
        # Remove lines that contains <init> tags
        log <- log[!grepl("^.*<init>.*",log)]
        # Substitute template barcode serial number with actual serial number
        log[1] <- gsub("SN='.*?'[[:blank:]]*", paste0("SN='", testID, "' "), log[1])
        # Fix bullshit
        log <- sub('results_table_intermediategtin',
                   'results_table_intermediate gtin',
                   log)
        log <- sub('results_tablegtin',
                   'results_table gtin',
                   log)
        log <- sub('check_resultsgtin',
                   'check_results gtin',
                   log)
        log <- sub(" universal='false''",
                   "' universal='false'",
                   log)
        log <- sub(" universal='true''",
                   "' universal='true'",
                   log)
        return(log)
      } else {
        log <- NULL
        return(log)
      }
    },
    error = function(cond){
      problem <- NULL
      return(problem)
    }
    
  )
  return(log)
}

RCA.XMLlog.get <- function(RCA_config,
                           testID,
                           out,
                           csv_folder,
                           RCA_file,
                           RCA_conf_mgmt,
                           RCA,
                           harness,
                           calibration,
                           config,
                           barcode,
                           ignoreISflag) {
  path_XML_RCA_log <- file.path(out,  RCA_config)
  file_XML_RCA_log <- paste0(path_XML_RCA_log,  testID, ".xml")
  
  if(file.exists(file_XML_RCA_log)){
    log <-  scan(file_XML_RCA_log, what="", sep="\n") 
  } else {
    # create dir if it does not exists
    if (!file.exists(path_XML_RCA_log)){ 
      dir.create(path_XML_RCA_log) 
    } 
    # Convert .csv raw data to a clean XML RCA log file and write it down on
    # data cache
    log <- RCA.csv.to.XMLlog(testID,
                             RCA_config,
                             csv_folder,
                             RCA_file,
                             RCA_conf_mgmt,
                             RCA,
                             harness,
                             calibration,
                             config,
                             barcode,
                             ignoreISflag)
    
    if (!is.null(log)){
      write(log, file.path(path_XML_RCA_log,  paste0(testID, ".xml"))) 
      return(log)
    }else {
      return(NULL)
    }
  }
}

RCA.targets.get <- function(log) {
  
  result_table <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                                       '//results_table//target'),
                                            xmlAttrs)),
                                stringsAsFactors = FALSE)
  if (length(result_table) == 0){
    df <- data.frame(comment = 'No data')
  } else{
    result_table <- result_table %>% 
      mutate(
        analytes = !bitwAnd(4096, as.numeric(flags)) > 0,
        type = case_when(analytes == TRUE ~ 'ANALYTE',
                         TRUE ~ 'CONTROL')
      ) %>%
      select(-analytes)
    
    target_cfg <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                                       '//Amplification//Targets//Target'),
                                            xmlAttrs)),
                                stringsAsFactors = FALSE)
    
    # merge results table with RCA config to get index to rc/ch match  
    targets_tmp <- merge(result_table, target_cfg) %>% 
      rename('rc' = 'reactionChamber',
             'ch' = 'channel' ) %>% 
      mutate(result = factor(result,
                             levels = c('$negative',
                                        '$positive',
                                        '$failed',
                                        '$na',
                                        '$invalid'),
                             labels = c('Neg',
                                        'Pos',
                                        'Fail',
                                        'NotAp',
                                        'Invalid'))) %>% 
      mutate_at(vars('ep', 'ct'),
                function(x){return(as.numeric(x)/100)})
    
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='8' and @codeblock='Linear Fit and Residuals Check']"),
                                    xmlAttrs)),
                        stringsAsFactors = FALSE)
    
    if(grepl('The normalized residual associated with', df$text)){
      
      df$linear_res <- as.numeric(gsub('^.*?([0-9][.][0-9].*)',
                                       '\\1',
                                       df$text))
      targets_tmp <- merge(targets_tmp,
                           df %>% select(rc, ch, linear_res),
                           all.x = TRUE)
    }
    
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='8' and @codeblock='Parametric Curve Residuals Check']"),
                                    xmlAttrs)),
                        stringsAsFactors=FALSE) %>% 
      mutate(logistic_res = as.numeric(sub('^.*?([0-9]+[.][0-9]+).*',
                                           '\\1',
                                           text))) %>% 
      select(rc, ch, logistic_res)
    
    
    targets_tmp <- merge(targets_tmp,
                         df ,
                         all.x = TRUE)
    
    # Recover intermediate ct and ep 
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='11' and @codeblock='Cycle Threshold Finding']"),
                                    xmlAttrs)),
                        stringsAsFactors=FALSE) %>% 
      mutate(ct_cp11 = gsub('^.*?([-]?[0-9]+[.][0-9]+).*',
                            '\\1',
                            text)) %>% 
      select(rc, ch, ct_cp11)
    
    
    targets_tmp <- merge(targets_tmp,
                         df,
                         all.x = TRUE)
    
    
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='12' and @codeblock='Final Endpoint Finder']"),
                                    xmlAttrs)),
                        stringsAsFactors = FALSE)
    
    if (length(df) > 0){
      
      df <- df %>% 
        filter(grepl("Successfully found final endpoint", df$text)) %>% 
        mutate(ep_cp12 = gsub('^.*?([-]?[0-9]+[.][0-9]+).*',
                              '\\1',
                              text)) %>% 
        select(rc, ch, ep_cp12)
      
      targets_tmp <- merge(targets_tmp,
                           df,
                           all.x = TRUE) 
    }
    
    # Recover obvious negative ep 
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='7' and @codeblock='Obvious Negative Detection']"),
                                    xmlAttrs)),
                        stringsAsFactors = FALSE) %>% 
      mutate(ep_cp7 = as.numeric(gsub('.*?([-]?[0-9]+[.][0-9]+).*',
                                      '\\1',
                                      text))) %>% 
      select(rc, ch, ep_cp7)
    
    targets_tmp <- merge(targets_tmp,
                         df,
                         all.x = TRUE)
    
    # Recover linear fit slope 
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='8' and @codeblock='Linear Fit and Residuals Check']"),
                                    xmlAttrs)),
                        stringsAsFactors=FALSE) %>% 
      mutate(lin_intercept = as.numeric(
        gsub('^.*intercept is .*?([-]?[0-9]+[.][0-9]+).*',
             '\\1',
             text))) %>% 
      select(rc, ch, lin_intercept)
    
    targets_tmp <- merge(targets_tmp,
                         df,
                         all.x = TRUE)
    
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='8' and @codeblock='Linear Fit and Residuals Check']"),
                                    xmlAttrs)),
                        stringsAsFactors = FALSE) %>% 
      mutate(lin_slope = as.numeric(sub('^.*slope ([-]?[0-9]+[.][0-9]+).*',
                                        '\\1',
                                        text,
                                        perl = TRUE))) %>% 
      select(rc, ch, lin_slope)
    
    targets_tmp <- merge(targets_tmp,
                         df,
                         all.x = TRUE)
    
    # Recover baseline before obvious negative detector
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='6' and @codeblock='Baseline Finding']"),
                                    xmlAttrs)),
                        stringsAsFactors = FALSE) %>% 
      mutate(baseline_pre = as.numeric(gsub(
        '^.*intercept is .*?([-]?[0-9]+[.][0-9]+).*',
        '\\1',
        text))) %>% 
      select(rc, ch, baseline_pre)
    
    targets_tmp <- merge(targets_tmp,
                         df,
                         all.x = TRUE)
    
    # Recover dEP, Ct, RMSE, dEP/RMSE at CP 8
    
    df <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                               "//msg[@cp='8' and @codeblock='Parametric Curve Fitting']"),
                                    xmlAttrs)),
                        stringsAsFactors = FALSE) %>% 
      mutate(cp8_dEP = as.numeric(
        gsub('^.*deltaEP [=] ([0-9]*[.]?[0-9]*).*$',
             '\\1',
             text)),
        cp8_ct = as.numeric(
          gsub('^.* ct [=] ([0-9]*[.]?[0-9]*).*$',
               '\\1',
               text)),
        cp8_rmse = as.numeric(
          gsub('^.* rmse [=] ([0-9]*[.]?[0-9]*), dEp.*$',
               '\\1',
               text)),
        cp8_snr = as.numeric(
          sub('^.* dEp div rmse [=][ ]*([0-9]*[.]?[0-9]*).*$',
              '\\1',
              text))) %>% 
      select(rc, ch, cp8_dEP, cp8_ct, cp8_rmse, cp8_snr)
    
    targets_tmp <- merge(targets_tmp,
                         df,
                         all.x = TRUE) %>% 
      filter(target_idx == index)
    
    return(targets_tmp) 
  }
}

RCA.amplifications.get <- function(log, run = FALSE) {
  if(run){
    return(data.frame(comment = 'No data'))
  } else{
    # Raw data amplification plots  ####
    raw <- NULL
    LUT <- c('Raw Data Read', 'Raw Data Transform');
    names(LUT) <- c('raw', 'transform')
    for (var in LUT){
      raw_nodes <- getNodeSet(log,
                              paste0("//data_transform//msg[@codeblock='",
                                     var,
                                     "']//readings"))
      
      tmp <- as.data.frame(t(xmlSApply(raw_nodes,xmlAttrs)),
                           stringsAsFactors = FALSE) %>% 
        select(cycle, values, matches('measurament'))
      
      # split per cycle values in columns and reshape to long
      tmp <- cbind(tmp,  colsplit(tmp$values, pattern = "[,]",
                                  names = paste0('X_',
                                                 rep(0:5, each=8),
                                                 paste0('', seq(1,8))))) %>% 
        mutate(values = NULL) %>%
        {if('measurament' %in% names(.)){
          melt(.,
               id.vars = c('measurament', 'cycle'))
        } else{
          melt(.,
               id.vars = 'cycle')
        }} %>%
        mutate_at(vars(cycle), as.numeric) %>% 
        mutate(ch = substr(gsub("X[_]", "", variable), 1, 1),
               rc = substr(gsub("X[_]", "", variable), 2, 2)) %>% 
        select(-matches('variable')) %>% 
        distinct() %>% 
        mutate(plot = names(LUT)[which(LUT==var)])
      
      raw <- bind_rows(raw, tmp)
    }
    
    # Adjusted Amplifications plots ####
    cycles <- max(raw$cycle)
    
    adjusted <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                                     "//data_transform//plot"),
                                          xmlAttrs)),
                              stringsAsFactors = FALSE) %>% 
      rename('plot' = 'codeblock') %>% 
      mutate(plot = 'adjusted') %>% 
      cbind(colsplit(.$values,
                     pattern = "[,]",
                     names = paste0("X_", 1:cycles))) %>% 
      mutate(values = NULL) %>% 
      melt(id.vars = c('plot', 'rc', 'ch'),
           variable.name = 'cycle') %>% 
      mutate(cycle = as.numeric(gsub('^[X][_]', '', cycle)))
    
    # Final amplification plots ####
    LUT <-  c('noise evaluation', 'outlier detection', 'InitialPlot', 'FinalPlot')
    names(LUT) <- c('noise', 'outlier', 'initial', 'final')
    
    check_results_all <- NULL
    for (var in LUT){
      plt <- as.data.frame(t(xmlSApply(getNodeSet(log,
                                                  paste0("//check_results//msg//plot[@codeblock='",
                                                         var,
                                                         "']")),
                                       xmlAttrs)),
                           stringsAsFactor = FALSE) 
      
      if (nrow(plt)>1){  #check that there is final amplification plot data
        plt <- plt %>% rename('plot' = 'codeblock')
        cmbs <- plt %>% distinct(rc, ch)
        
        for(i in 1:nrow(cmbs)){
          tmp <- plt %>% 
            filter(rc == cmbs$rc[i],
                   ch == cmbs$ch[i])
          
          # capture x axis, for cycle values and remove x axis
          x.axis.idx <- which(tmp$axis == 'x')
          x.colnames <- paste0("X_", unlist(strsplit(
            as.character(tmp$values[x.axis.idx][1]), ", "))) # further check
          tmp <- tmp[-x.axis.idx,]
          
          ## split per cycle values in columns and reshape to long
          tmp <- cbind(tmp,  colsplit(tmp$values, pattern = "[,]",
                                      names = x.colnames)) %>% 
            select(-values) %>% 
            melt(id.vars = c("plot", "rc", "ch", "axis")) %>% 
            mutate(plot = paste0(plot, '_', axis)) %>% 
            select(-axis) %>% 
            rename('cycle' = 'variable') %>% 
            mutate(cycle = as.numeric(gsub("^[X][_]", "", cycle)))
          
          check_results_all <- bind_rows(check_results_all, tmp)
        }
        
      }
    }
    return(bind_rows(raw, adjusted, check_results_all))
  }
  
}
