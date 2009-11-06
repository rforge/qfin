\name{sstl}
\alias{sstl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sstl(SERIES, FREQ = 365.24)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SERIES}{ ~~Describe \code{SERIES} here~~ }
  \item{FREQ}{ ~~Describe \code{FREQ} here~~ }
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
function(SERIES, FREQ = 365.24000000000001)
{
    if(class(SERIES) != "timeSeries")
        stop("The function sstl is restricted to timeSeries objects")
    SDATE <- timeDate(as.character(min(positions(SERIES))), format =
        "\%02m/\%02d/\%Y")
    YR <- as.integer(as.character(timeDate(as.character(SDATE), format =
        "\%Y")))
    MO <- as.integer(as.character(timeDate(as.character(SDATE), format =
        "\%02m")))
    DAY <- as.integer(as.character(timeDate(as.character(SDATE), format =
        "\%02d")))
    TMP <- julian(m = MO, d = DAY, y = YR, origin = c(m = 1, d = 1, y = YR
        ))
    START <- YR + (1 + TMP)/365
    TMP.ts <- ts(seriesData(SERIES), start = START, frequency = FREQ)
    TMP.stl <- stl(TMP.ts, "periodic")
    OUT <- list(sea = timeSeries(pos = positions(SERIES), data = TMP.stl$
        sea), rem = timeSeries(pos = positions(SERIES), data = TMP.stl$
        rem))
    class(OUT) <- "sstl"
    OUT
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
