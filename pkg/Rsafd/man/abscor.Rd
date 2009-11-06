\name{abscor}
\alias{abscor}
\alias{axcor}
\alias{squarecor}

\title{Auto-correlation matrices of the future and past increments of a time series}
\description{
 For a given timeSeries X, \code{axcor} compute the auto-correlation matrix of the 4 timeSeries, X[t+5]-X[t], X[t]-X[t-5], X[t]-X[t-15],X[t]-X[t-60], while \code{abscor} compute the auto-correlation matrix of the absolute values of these increments, and \code{squarecor} compute the auto-correlation matrix of the absolute values of these increments.
}
\usage{
abscor(X), axcor(X), squarecor(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a scalar timeSeries or a numeric vector }
}
\details{
}
\value{
  A 4 x 4 correlation matrix
}

\author{Rene Carmona}

\seealso{\code{\link{der}},\code{\link{axcor}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
