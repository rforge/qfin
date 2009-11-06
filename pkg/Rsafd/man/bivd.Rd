\name{bivd}
\alias{bivd}
\alias{contour.dbivd}
\alias{contour.pbivd}
\alias{persp.dbivd}

\title{Bivariate Distribution}
\description{
\item{bivd}{Create an object of Bivariate Distribution class }
\item{contour.dbivd}{Create a contour plot of the density of a bivariate distribution}
\item{contour.pbivd}{Create a contour plot of the cumulative distribution function (c.d.f.)  of a bivariate distribution}
\item{persp.dbivd}{Create a surface plot of the density of a bivariate distribution}}
\usage{
\item{bivd}{bivd(cop, marginX = "unif", marginY = marginX, param.marginX = c(0, 1), param.marginY = param.marginX)}
\item{contour.dbivd}{contour.dbivd(dist, n = 50, xlim = NA, ylim = NA, ...)}
\item{contour.pbivd}{contour.pbivd(dist, n = 50, xlim = NA, ylim = NA, ...)}
}
\arguments{
  \item{cop}{an object of class copula, e.g. given by the name of the copula family and the values parameters of the copula}
  \item{marginX}{A string giving the name of the univariate distribution family of the first marginal}
  \item{marginY}{A string giving the name of the univariate distribution family of the second marginal}
  \item{param.marginX}{Numeric vector giving the numerical values of the parameters of the first marginal }
  \item{param.marginY}{Numeric vector giving the numerical values of the parameters of the second marginal  }
  \item{dist}{a bivariate distribution (as returned by \code{bivd} }
  \item{n}{The number of levels plotted}
  \item{xlim}{Vector of the limits on the x-axis }
  \item{ylim}{Vector of the limits on the y-axis }
}
\details{
}
\value{
  A Bivariate Distribution in the case of \code{bivd} while the functions \code{contour.dbivd}, \code{contour.pbivd} and \code{persp.dbivd} are used for their side-effects, namely producing contour and 3-D surface plots.
}
\references{R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag}
\author{Rene Carmona }

\examples{
BIV1 <- bivd(gumbel.copula(1.4), "norm", "t", c(3,4), 3)
persp.dbivd(BIV1)

par(mfrow=c(1,2))
contour.dbivd(BIV1)
contour.pbivd(BIV1)
par(mfrow=c(1,1))
}

\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
