\name{generateCartridge}
\alias{generateCartridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generateCartridge(list, in2data.url = "https://in2data.qiagen.com/in2data/backend/api/", fromTime = "1980-01-01")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{
%%     ~~Describe \code{list} here~~
}
  \item{in2data.url}{
%%     ~~Describe \code{in2data.url} here~~
}
  \item{fromTime}{
%%     ~~Describe \code{fromTime} here~~
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
function (list, in2data.url = "https://in2data.qiagen.com/in2data/backend/api/", 
    fromTime = "1980-01-01") 
{
    fromTime <- format(as.Date(fromTime), "\%Y-\%m-\%dT\%H:\%M:\%S.000Z")
    Token <- generateToken()
    results.page <- POST(paste0(in2data.url, "results/0/50"), 
        timeout(30), add_headers(`Content-Type` = "application/json", 
            Authorization = Token), body = paste0("{\"filters\":{\"cartridgeSerialNumber\":{\"value\":\"", 
            paste(list$testID, collapse = "\\n"), "\",\"operation\":\"CONTAINS\"},", 
            "\"startTimestamp\":{\"value\":{\"from\":\"", fromTime, 
            "\"", "},\"operation\":\"EQ\"}", "}}"))
    calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 
        1
    while (results.page$status_code == 429) {
        Sys.sleep(0.5)
        results.page <- POST(paste0(in2data.url, "results/0/50"), 
            timeout(30), add_headers(`Content-Type` = "application/json", 
                Authorization = Token), body = paste0("{\"filters\":{\"cartridgeSerialNumber\":{\"value\":\"", 
                paste(list$testID, collapse = "\\n"), "\",\"operation\":\"CONTAINS\"},", 
                "\"startTimestamp\":{\"value\":{\"from\":\"", 
                fromTime, "\"", "},\"operation\":\"EQ\"}", "}}"))
        Token <- unname(results.page$request$headers["Authorization"])
        calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 
            1
    }
    Sys.sleep(1/calls_remaining)
    tests <- NULL
    n_pages <- content(results.page)$numberOfPages - 1
    for (page in seq(0, n_pages)) {
        results.page <- POST(paste0(in2data.url, paste0("results/", 
            page, "/50")), timeout(30), add_headers(`Content-Type` = "application/json", 
            Authorization = Token), body = paste0("{\"filters\":{\"cartridgeSerialNumber\":{\"value\":\"", 
            paste(list$testID, collapse = "\\n"), "\",\"operation\":\"CONTAINS\"},", 
            "\"startTimestamp\":{\"value\":{\"from\":\"", fromTime, 
            "\"", "},\"operation\":\"EQ\"}", "}}"))
        print(body)
        print(results.page$status_code)
        Token <- unname(results.page$request$headers["Authorization"])
        calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 
            1
        while (results.page$status_code == 429) {
            Sys.sleep(0.5)
            results.page <- POST(paste0(in2data.url, paste0("results/", 
                page, "/50")), timeout(30), add_headers(`Content-Type` = "application/json", 
                Authorization = Token), body = paste0("{\"filters\":{\"cartridgeSerialNumber\":{\"value\":\"", 
                paste(list$testID, collapse = "\\n"), "\",\"operation\":\"CONTAINS\"},", 
                "\"startTimestamp\":{\"value\":{\"from\":\"", 
                fromTime, "\"", "},\"operation\":\"EQ\"}", "}}"))
            Token <- unname(results.page$request$headers["Authorization"])
            calls_remaining <- as.numeric(results.page$headers$`x-rate-limit-remaining`) + 
                1
        }
        Sys.sleep(1/calls_remaining)
        test.page <- content(results.page)$content \%>\% lapply(function(x) {
            x \%>\% unlist() \%>\% as.data.frame() \%>\% t() \%>\% as.data.frame()
        }) \%>\% bind_rows() \%>\% mutate_all(as.character) \%>\% mutate(uid = case_when(cartridgeSerialNumber == 
            "P00000007" ~ sampleIdentifier, TRUE ~ cartridgeSerialNumber)) \%>\% 
            rename(cartridge_SN = cartridgeSerialNumber, sample_id = sampleIdentifier, 
                start_time = startTimestamp, AM = amName, OM = omName, 
                ADF_name = assayName, ADF_version = assayVersion, 
                error_code = errorCode, IC = controlsPassed, 
                AM_firmware = amVersion, OM_version = omVersion, 
                Barcode = barcode, Patient_ID = patientIdentifier, 
                total_result = totalResult, sample_type = "sampleType.name") \%>\% 
            mutate(total_result = case_when(total_result == "POSITIVE" ~ 
                "pos", total_result == "POSITIVE_WITH_WARNINGS" ~ 
                "pos*", total_result == "NEGATIVE" ~ "neg", total_result == 
                "FAILED" ~ "fail")) \%>\% select(uid, id, cartridge_SN, 
            sample_id, start_time, AM, OM, ADF_name, ADF_version, 
            total_result, error_code, IC, AM_firmware, OM_version, 
            Barcode, Patient_ID, sample_type, contains("result"), 
            contains("ct"), contains("ep"), contains("label"), 
            everything())
        print(paste0("Page ", 1 + page, " out of ", n_pages + 
            1))
    }
    return(tests)
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
