\name{VaR.exp.sim}
\alias{VaR.exp.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VaR.exp.sim(n, Q, copula, x.est, y.est, lambda1, lambda2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{Q}{ ~~Describe \code{Q} here~~ }
  \item{copula}{ ~~Describe \code{copula} here~~ }
  \item{x.est}{ ~~Describe \code{x.est} here~~ }
  \item{y.est}{ ~~Describe \code{y.est} here~~ }
  \item{lambda1}{ ~~Describe \code{lambda1} here~~ }
  \item{lambda2}{ ~~Describe \code{lambda2} here~~ }
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
function(n, Q, copula, x.est, y.est, lambda1, lambda2 ) 
{
    sim.uv <- rcopula(copula, n)
    Xsim <- gpd.2q(sim.uv$x,x.est)
    Ysim <- gpd.2q(sim.uv$y,y.est)
    ww <- log(lambda1*exp(Xsim) + lambda2*exp(Ysim))
    varv <- -as.vector(quantile(ww,Q))
    esf <- vector(length = length(Q), mode = "numeric")
    for (i in 1:length(Q))  esf[i] <- -mean(ww[ww < -varv[i]])

    val <- c(n,varv,esf)
    nn <-  vector(length = length(Q)*2 + 1, mode = "character")
    nn[1] <- "Simulation size"
    nn[2:(length(Q)+1)] <- paste("VaR Q=",Q,sep = "")
    nn[(length(Q)+2):(2*length(Q)+1)] <- paste("ES Q=",Q,sep = "")
    names(val) <- nn
    val
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
