\name{RCA.XMLlog.get}
\alias{RCA.XMLlog.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RCA.XMLlog.get(RCA_config, testID, out, csv_folder, RCA_file, RCA_conf_mgmt, RCA, harness, calibration, config, barcode, ignoreISflag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RCA_config}{
%%     ~~Describe \code{RCA_config} here~~
}
  \item{testID}{
%%     ~~Describe \code{testID} here~~
}
  \item{out}{
%%     ~~Describe \code{out} here~~
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
function (RCA_config, testID, out, csv_folder, RCA_file, RCA_conf_mgmt, 
    RCA, harness, calibration, config, barcode, ignoreISflag) 
{
    path_XML_RCA_log <- file.path(out, RCA_config)
    file_XML_RCA_log <- paste0(path_XML_RCA_log, testID, ".xml")
    if (file.exists(file_XML_RCA_log)) {
        log <- scan(file_XML_RCA_log, what = "", sep = "\n")
    }
    else {
        if (!file.exists(path_XML_RCA_log)) {
            dir.create(path_XML_RCA_log)
        }
        log <- RCA.csv.to.XMLlog(testID, RCA_config, csv_folder, 
            RCA_file, RCA_conf_mgmt, RCA, harness, calibration, 
            config, barcode, ignoreISflag)
        if (!is.null(log)) {
            write(log, file.path(path_XML_RCA_log, paste0(testID, 
                ".xml")))
            return(log)
        }
        else {
            return(NULL)
        }
    }
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
