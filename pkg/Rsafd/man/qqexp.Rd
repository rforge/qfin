\name{qqexp}
\alias{qqexp}

\title{Q-Q PLOT WITH THE EXPONENTIAL DISTRIBUTION}
\description{
Produce an exponential Q-Q plot by comparing the data sample to a sample of the same size of the exponential distribution}

\usage{
qqexp(x)
}

\arguments{
  \item{x}{Data numeric vector}
}
\details{
Generate a random sample from the exponential distribution of the same length as the data vector \code{x}, and produce an empirical Q-Q plot of the two samples, together with the line where the quantile-quatile points should have been, had the true distribution of the data vector be exponential.
}
\value{
  Used for its (graphical) side effects.
}


\author{Rene Carmona, \email{rcarmona@princeton.edu}}

\seealso{     
\code{\link{qqnorm}}
 }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
}

\keyword{distribution}
\keyword{Q-Q plot}
