\name{RCA.amplifications.get}
\alias{RCA.amplifications.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RCA.amplifications.get(log, run = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{run}{
%%     ~~Describe \code{run} here~~
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
function (log, run = FALSE) 
{
    if (run) {
        return(data.frame(comment = "No data"))
    }
    else {
        raw <- NULL
        LUT <- c("Raw Data Read", "Raw Data Transform")
        names(LUT) <- c("raw", "transform")
        for (var in LUT) {
            raw_nodes <- getNodeSet(log, paste0("//data_transform//msg[@codeblock='", 
                var, "']//readings"))
            tmp <- as.data.frame(t(xmlSApply(raw_nodes, xmlAttrs)), 
                stringsAsFactors = FALSE) \%>\% select(cycle, values, 
                matches("measurament"))
            tmp <- cbind(tmp, colsplit(tmp$values, pattern = "[,]", 
                names = paste0("X_", rep(0:5, each = 8), paste0("", 
                  seq(1, 8))))) \%>\% mutate(values = NULL) \%>\% 
                {
                  if ("measurament" \%in\% names(.)) {
                    melt(., id.vars = c("measurament", "cycle"))
                  }
                  else {
                    melt(., id.vars = "cycle")
                  }
                } \%>\% mutate_at(vars(cycle), as.numeric) \%>\% 
                mutate(ch = substr(gsub("X[_]", "", variable), 
                  1, 1), rc = substr(gsub("X[_]", "", variable), 
                  2, 2)) \%>\% select(-matches("variable")) \%>\% 
                distinct() \%>\% mutate(plot = names(LUT)[which(LUT == 
                var)])
            raw <- bind_rows(raw, tmp)
        }
        cycles <- max(raw$cycle)
        adjusted <- as.data.frame(t(xmlSApply(getNodeSet(log, 
            "//data_transform//plot"), xmlAttrs)), stringsAsFactors = FALSE) \%>\% 
            rename(plot = "codeblock") \%>\% mutate(plot = "adjusted") \%>\% 
            cbind(colsplit(.$values, pattern = "[,]", names = paste0("X_", 
                1:cycles))) \%>\% mutate(values = NULL) \%>\% melt(id.vars = c("plot", 
            "rc", "ch"), variable.name = "cycle") \%>\% mutate(cycle = as.numeric(gsub("^[X][_]", 
            "", cycle)))
        LUT <- c("noise evaluation", "outlier detection", "InitialPlot", 
            "FinalPlot")
        names(LUT) <- c("noise", "outlier", "initial", "final")
        check_results_all <- NULL
        for (var in LUT) {
            plt <- as.data.frame(t(xmlSApply(getNodeSet(log, 
                paste0("//check_results//msg//plot[@codeblock='", 
                  var, "']")), xmlAttrs)), stringsAsFactor = FALSE)
            if (nrow(plt) > 1) {
                plt <- plt \%>\% rename(plot = "codeblock")
                cmbs <- plt \%>\% distinct(rc, ch)
                for (i in 1:nrow(cmbs)) {
                  tmp <- plt \%>\% filter(rc == cmbs$rc[i], ch == 
                    cmbs$ch[i])
                  x.axis.idx <- which(tmp$axis == "x")
                  x.colnames <- paste0("X_", unlist(strsplit(as.character(tmp$values[x.axis.idx][1]), 
                    ", ")))
                  tmp <- tmp[-x.axis.idx, ]
                  tmp <- cbind(tmp, colsplit(tmp$values, pattern = "[,]", 
                    names = x.colnames)) \%>\% select(-values) \%>\% 
                    melt(id.vars = c("plot", "rc", "ch", "axis")) \%>\% 
                    mutate(plot = paste0(plot, "_", axis)) \%>\% 
                    select(-axis) \%>\% rename(cycle = "variable") \%>\% 
                    mutate(cycle = as.numeric(gsub("^[X][_]", 
                      "", cycle)))
                  check_results_all <- bind_rows(check_results_all, 
                    tmp)
                }
            }
        }
        return(bind_rows(raw, adjusted, check_results_all))
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
