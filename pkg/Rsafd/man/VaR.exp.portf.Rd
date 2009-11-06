\name{VaR.exp.portf}
\alias{VaR.exp.portf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VaR.exp.portf(Q, copula, x.est, y.est, lambda1, lambda2, range, subdivisions = 1000, tol.f = 5e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q}{ ~~Describe \code{Q} here~~ }
  \item{copula}{ ~~Describe \code{copula} here~~ }
  \item{x.est}{ ~~Describe \code{x.est} here~~ }
  \item{y.est}{ ~~Describe \code{y.est} here~~ }
  \item{lambda1}{ ~~Describe \code{lambda1} here~~ }
  \item{lambda2}{ ~~Describe \code{lambda2} here~~ }
  \item{range}{ ~~Describe \code{range} here~~ }
  \item{subdivisions}{ ~~Describe \code{subdivisions} here~~ }
  \item{tol.f}{ ~~Describe \code{tol.f} here~~ }
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
function(Q, copula, x.est, y.est, lambda1, lambda2,
      range, subdivisions = 1000, tol.f = 5e-5)   
{     
     uniroot(fbar.exp.portf, interval=range, copula = copula,x.est = x.est, y.est = y.est,
       lambda1 = lambda1, lambda2 = lambda2, q = Q,
          subdivisions = subdivisions,tol.f=tol.f)$root
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
