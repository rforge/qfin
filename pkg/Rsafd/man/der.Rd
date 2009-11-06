\name{der}
\alias{der}

\title{Derivative of a timeSeries }
\description{
Computes the derivative of a timeSeries object}
\usage{
der(X, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Object of class timeSeries}
  \item{k}{integer giving the number of lags used in the derivation}
}
\details{

}
\value{
The timeSeries Y defined by Y[t]=X[t]-X[t-k]}
\references{ ~put references to the literature/web site here ~ }
\author{Rene Carmona}
\seealso{\code{\link{axcor}}, \code{\link{abscor}}, }
\examples{
}

\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
