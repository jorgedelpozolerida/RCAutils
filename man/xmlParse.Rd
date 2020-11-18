\name{xmlParse}
\alias{xmlParse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xmlParse(xml)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xml}{
%%     ~~Describe \code{xml} here~~
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
function (xml) 
{
    out <- tryCatch({
        xmlTreeParse(xml, useInternalNodes = TRUE)
    }, error = function(cond) {
        problem <- as.data.frame(xml) \%>\% filter_all(~grepl("fail", 
            ., ignore.case = TRUE))
        problem <- as.character(problem[1, 1])
        problem <- data.frame(garbage = unlist(str_extract_all(problem, 
            "[a-zA-Z]+='[ .,=0-9a-zA-Z]+'"))) \%>\% separate(garbage, 
            c("tag", "value"), sep = "=", extra = "merge") \%>\% 
            as.data.frame(row.names = .$tag) \%>\% select(-tag)
        if (all(is.na(problem))) {
            return("Not enough data. Cannot coerce")
        }
        else {
            problem <- as.data.frame(t(problem)) \%>\% mutate_all(~str_replace_all(., 
                "'", "")) \%>\% select(any_of(c("cp", "codeblock", 
                "rc", "ch", "result", "text")))
            return(problem)
        }
    })
    return(out)
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
