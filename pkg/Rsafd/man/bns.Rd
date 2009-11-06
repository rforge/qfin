\name{bns}
\alias{bns}
\alias{fns}
\alias{yns}

\title{Coupon Bond Price and Yield in the Nelson - Siegel Family}
\description{
  Computations in the Nelson - Siegel family of forward curves.
}
\usage{
\item{fns}{\code{fns(x, THETA)}}
\item{yns}{\code{yns(x, THETA)}}
\item{bns}{\code{bns(COUPON, AI, LIFE, X = 100, THETA = c(0.06, 0, 0, 1))}}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Time to maturity (positive numeric)}
  \item{THETA}{Vector of the four parameters of a forward curve from the Nelson - Siegel family}  
  \item{COUPON}{Rate of the semi-annual coupon payment}
  \item{AI}{Accrued Interest}
  \item{LIFE}{Time to maturity of the bond}
  \item{X}{Nominal of the bond}
}

\value{
\item{fns}{ numerical value of the Nelson - Siegel forward curve determined by the parameter THETA for the time to maturity x}
\item{yns}{numerical value of the Nelson - Siegel yield curve determined by the parameter THETA for the time to maturity x}
\item{bns}{\code{bns(COUPON, AI, LIFE, X = 100, THETA = c(0.06, 0, 0, 1))}}
}
\references{R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag}
\author{Rene Carmona}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
}
