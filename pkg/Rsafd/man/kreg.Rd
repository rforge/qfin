\name{kreg}
\alias{kreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kreg(x, y, xpred = x, kernel = 4, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{xpred}{ ~~Describe \code{xpred} here~~ }
  \item{kernel}{ ~~Describe \code{kernel} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
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
function(x, y, xpred = x, kernel = 4, b)
{
    ########################################################################
    # kreg
    #       ----
    # kernel regression (possibly multivariate)
    #
    # Inputs
    # ------
    # x  nxp matrix of n observations & p explanatory variables
    # y  vector of n observations of the response variable
    # xpred n'xp matrix of n' regressors for prediction
    # kernel function
    #        1 <-> uniform
    #        2 <-> cosine
    #        3 <-> triangular
    #        4 <-> Epanechnikov
    #        5 <-> quartic
    #        6 <-> triweight
    #        7 <-> Gaussian
    # b  bandwidth
    #
    # Outputs
    # -------
    # xpred  same as in input
    # ypred  vector of the predictions (for the rows of xpred)
    # b      same as in input
    #
    ###########################################################################
    if(is.vector(x)) {
        n <- length(x)
        p <- 1
    }
    else if(is.matrix(x)) {
        n <- dim(x)[1]
        p <- dim(x)[2]
    }
    if(is.vector(xpred)) {
        npred <- length(xpred)
        ppred <- 1
    }
    else if(is.matrix(xpred)) {
        npred <- dim(xpred)[1]
        ppred <- dim(xpred)[2]
    }
    if(is.vector(y)) {
        ny <- length(y)
        py <- 1
    }
    else if(is.matrix(y)) {
        ny <- dim(y)[1]
        py <- dim(y)[2]
    }
    if(ny != n) {
        stop("The number of rows of x should be the same\n  as the number of rows of y"
            )
    }
    if(py != 1) {
        stop("y should not have more than one column")
    }
    if(p != ppred) {
        stop("The number of columns of x should be the same\n  as the number of columns of xpred"
            )
    }
    ypred <- rep(0, npred)
    x <- c(x)
    dim(x) <- c(length(x), 1)
    dim(y) <- c(length(y), 1)
    xxpred <- c(xpred)
    dim(xxpred) <- c(length(xxpred), 1)
    dim(ypred) <- c(length(ypred), 1)
    z <- .C("kreg",
        as.double(x),
        as.double(y),
        as.double(xxpred),
        ypred = as.double(ypred),
        as.integer(n),
        as.integer(p),
        as.integer(npred),
        as.integer(kernel),
        as.double(b))
    ypred <- z$ypred
    dim(ypred) <- c(npred, 1)
    list(xpred = xpred, ypred = ypred, b = b)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
