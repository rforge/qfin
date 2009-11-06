\name{kalman}
\alias{kalman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kalman(FF, SigV, GG, SigW, Xhat, Omega, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FF}{ ~~Describe \code{FF} here~~ }
  \item{SigV}{ ~~Describe \code{SigV} here~~ }
  \item{GG}{ ~~Describe \code{GG} here~~ }
  \item{SigW}{ ~~Describe \code{SigW} here~~ }
  \item{Xhat}{ ~~Describe \code{Xhat} here~~ }
  \item{Omega}{ ~~Describe \code{Omega} here~~ }
  \item{Y}{ ~~Describe \code{Y} here~~ }
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
function(FF, SigV, GG, SigW, Xhat, Omega, Y)
{
    Delta <- GG \%*\% Omega \%*\% t(GG) + SigW
    Theta <- FF \%*\% Omega \%*\% t(GG)
    X <- FF \%*\% Xhat + Theta \%*\% solve(Delta) \%*\% (Y - GG \%*\% Xhat)
    Om <- FF \%*\% Omega \%*\% t(FF) + SigV - Theta \%*\% solve(Delta) \%*\% t(
        Theta)
    Ret <- list(xpred = X, error = Om)
    Ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
