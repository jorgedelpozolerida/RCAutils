\name{RCA.targets.get}
\alias{RCA.targets.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RCA.targets.get(log)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{log}{
%%     ~~Describe \code{log} here~~
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
function (log) 
{
    result_table <- as.data.frame(t(xmlSApply(getNodeSet(log, 
        "//results_table//target"), xmlAttrs)), stringsAsFactors = FALSE)
    if (length(result_table) == 0) {
        df <- data.frame(comment = "No data")
    }
    else {
        result_table <- result_table \%>\% mutate(analytes = !bitwAnd(4096, 
            as.numeric(flags)) > 0, type = case_when(analytes == 
            TRUE ~ "ANALYTE", TRUE ~ "CONTROL")) \%>\% select(-analytes)
        target_cfg <- as.data.frame(t(xmlSApply(getNodeSet(log, 
            "//Amplification//Targets//Target"), xmlAttrs)), 
            stringsAsFactors = FALSE)
        targets_tmp <- merge(result_table, target_cfg) \%>\% rename(rc = "reactionChamber", 
            ch = "channel") \%>\% mutate(result = factor(result, 
            levels = c("$negative", "$positive", "$failed", "$na", 
                "$invalid"), labels = c("Neg", "Pos", "Fail", 
                "NotAp", "Invalid"))) \%>\% mutate_at(vars("ep", 
            "ct"), function(x) {
            return(as.numeric(x)/100)
        })
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='8' and @codeblock='Linear Fit and Residuals Check']"), 
            xmlAttrs)), stringsAsFactors = FALSE)
        if (grepl("The normalized residual associated with", 
            df$text)) {
            df$linear_res <- as.numeric(gsub("^.*?([0-9][.][0-9].*)", 
                "\\1", df$text))
            targets_tmp <- merge(targets_tmp, df \%>\% select(rc, 
                ch, linear_res), all.x = TRUE)
        }
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='8' and @codeblock='Parametric Curve Residuals Check']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(logistic_res = as.numeric(sub("^.*?([0-9]+[.][0-9]+).*", 
            "\\1", text))) \%>\% select(rc, ch, logistic_res)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='11' and @codeblock='Cycle Threshold Finding']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(ct_cp11 = gsub("^.*?([-]?[0-9]+[.][0-9]+).*", 
            "\\1", text)) \%>\% select(rc, ch, ct_cp11)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='12' and @codeblock='Final Endpoint Finder']"), 
            xmlAttrs)), stringsAsFactors = FALSE)
        if (length(df) > 0) {
            df <- df \%>\% filter(grepl("Successfully found final endpoint", 
                df$text)) \%>\% mutate(ep_cp12 = gsub("^.*?([-]?[0-9]+[.][0-9]+).*", 
                "\\1", text)) \%>\% select(rc, ch, ep_cp12)
            targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        }
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='7' and @codeblock='Obvious Negative Detection']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(ep_cp7 = as.numeric(gsub(".*?([-]?[0-9]+[.][0-9]+).*", 
            "\\1", text))) \%>\% select(rc, ch, ep_cp7)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='8' and @codeblock='Linear Fit and Residuals Check']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(lin_intercept = as.numeric(gsub("^.*intercept is .*?([-]?[0-9]+[.][0-9]+).*", 
            "\\1", text))) \%>\% select(rc, ch, lin_intercept)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='8' and @codeblock='Linear Fit and Residuals Check']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(lin_slope = as.numeric(sub("^.*slope ([-]?[0-9]+[.][0-9]+).*", 
            "\\1", text, perl = TRUE))) \%>\% select(rc, ch, lin_slope)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='6' and @codeblock='Baseline Finding']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(baseline_pre = as.numeric(gsub("^.*intercept is .*?([-]?[0-9]+[.][0-9]+).*", 
            "\\1", text))) \%>\% select(rc, ch, baseline_pre)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE)
        df <- as.data.frame(t(xmlSApply(getNodeSet(log, "//msg[@cp='8' and @codeblock='Parametric Curve Fitting']"), 
            xmlAttrs)), stringsAsFactors = FALSE) \%>\% mutate(cp8_dEP = as.numeric(gsub("^.*deltaEP [=] ([0-9]*[.]?[0-9]*).*$", 
            "\\1", text)), cp8_ct = as.numeric(gsub("^.* ct [=] ([0-9]*[.]?[0-9]*).*$", 
            "\\1", text)), cp8_rmse = as.numeric(gsub("^.* rmse [=] ([0-9]*[.]?[0-9]*), dEp.*$", 
            "\\1", text)), cp8_snr = as.numeric(sub("^.* dEp div rmse [=][ ]*([0-9]*[.]?[0-9]*).*$", 
            "\\1", text))) \%>\% select(rc, ch, cp8_dEP, cp8_ct, 
            cp8_rmse, cp8_snr)
        targets_tmp <- merge(targets_tmp, df, all.x = TRUE) \%>\% 
            filter(target_idx == index)
        return(targets_tmp)
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
