\name{makedate}
\alias{makedate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makedate(REAL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{REAL}{ ~~Describe \code{REAL} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(REAL)
{
    # This function takes a real like 19920106.00 and returns a date of the
    # form 10/6/1992
    YEAR <- floor(as.integer(REAL)/10000)
    TMP <- as.integer(REAL) - YEAR * 10000
    MONTH <- floor(TMP/100)
    DAY <- TMP - MONTH * 100
    TMP.julian <- julian(MONTH, DAY, YEAR)
    dates(TMP.julian)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
