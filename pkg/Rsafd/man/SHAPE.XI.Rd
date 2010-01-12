\name{SHAPE.XI}
\alias{SHAPE.XI}
\docType{data}

\title{PARAMETRIZATION OF GENERALIZED ETREME VALUES AND PARETO DISTRIBUTIONS}

\description{
      For financial applications, it is customary to
   define the GEV and GPD distributions as in the book
}
      
\note{
    The values of most of the functions of the library \code{htd}
    are affected by the value of the boolean variable SHAPE.XI. Its default value is TRUE
}

\examples{
m <- c(0,0,0)
lambda <- c(1.3, 1.0, .7)
xi <- c(-0.2,0.0,0.2)
x <- rep(1.0,3)
pgev(x,m,lambda,xi)
[1] 0.6480696 0.6922006 0.7522940
SHAPE.XI <- F
pgev(x,m,lambda,xi)
[1] 0.6132727 0.6922006 0.8303280
}

\author{Rene Carmona, \email{rcarmona@princeton.edu}}
\keyword{parameter}
