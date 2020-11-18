\name{RCA.csv.to.XMLlog}
\alias{RCA.csv.to.XMLlog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RCA.csv.to.XMLlog(testID, RCA_config, csv_folder, RCA_file, RCA_conf_mgmt, RCA, harness, calibration, config, barcode, ignoreISflag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{testID}{
%%     ~~Describe \code{testID} here~~
}
  \item{RCA_config}{
%%     ~~Describe \code{RCA_config} here~~
}
  \item{csv_folder}{
%%     ~~Describe \code{csv_folder} here~~
}
  \item{RCA_file}{
%%     ~~Describe \code{RCA_file} here~~
}
  \item{RCA_conf_mgmt}{
%%     ~~Describe \code{RCA_conf_mgmt} here~~
}
  \item{RCA}{
%%     ~~Describe \code{RCA} here~~
}
  \item{harness}{
%%     ~~Describe \code{harness} here~~
}
  \item{calibration}{
%%     ~~Describe \code{calibration} here~~
}
  \item{config}{
%%     ~~Describe \code{config} here~~
}
  \item{barcode}{
%%     ~~Describe \code{barcode} here~~
}
  \item{ignoreISflag}{
%%     ~~Describe \code{ignoreISflag} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (testID, RCA_config, csv_folder, RCA_file, RCA_conf_mgmt, 
    RCA, harness, calibration, config, barcode, ignoreISflag) 
{
    log <- tryCatch({
        if (file.exists(file.path(csv_folder, paste0(testID, 
            ".csv")))) {
            if (!any(c(is.null(RCA_config), is.null(RCA_file), 
                is.null(RCA_conf_mgmt)))) {
                RCA_cfg <- RCA.config.get(RCA_config, RCA_file, 
                  RCA_conf_mgmt)
                harness <- RCA_cfg$Harness
                RCA <- file.path(RCA_file, RCA_cfg$RCA)
                calibration <- file.path(RCA_file, RCA_cfg$HW)
                config <- file.path(RCA_file, RCA_cfg$RCAConfig)
                barcode <- file.path(RCA_file, RCA_cfg$Barcode)
                ignoreISflag <- ifelse(RCA_cfg$ignoreIS == 1, 
                  " -i ", "")
            }
            command <- paste0("cmd.exe /c java -Duser.language=EN -jar ", 
                RCA_file, harness, " -l FINE  -t ", RCA, " ", 
                calibration, " ", config, " ", barcode, " ", 
                file.path(csv_folder, paste0(testID, ".csv")), 
                " -s ", testID, ignoreISflag, " 2>&1")
            print(command)
            log <- system(command, intern = TRUE)
            log <- sub("^[^<]*((FINE(R|))|WARNING|INFO):*[[:blank:]]*((?=<)|(?=[^<]).*)", 
                "", log, perl = TRUE)
            log <- log[log != ""]
            log <- log[which(grepl("<RCA.*>.*", log)):which(grepl("</RCA>.*", 
                log))]
            log <- log[grepl("^.*<.*>.*", log)]
            log <- log[!grepl("^.*<init>.*", log)]
            log[1] <- gsub("SN='.*?'[[:blank:]]*", paste0("SN='", 
                testID, "' "), log[1])
            log <- sub("results_table_intermediategtin", "results_table_intermediate gtin", 
                log)
            log <- sub("results_tablegtin", "results_table gtin", 
                log)
            log <- sub("check_resultsgtin", "check_results gtin", 
                log)
            log <- sub(" universal='false''", "' universal='false'", 
                log)
            log <- sub(" universal='true''", "' universal='true'", 
                log)
            return(log)
        }
        else {
            log <- NULL
            return(log)
        }
    }, error = function(cond) {
        problem <- NULL
        return(problem)
    })
    return(log)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
