\name{begday}
\alias{begday}
\alias{noon}
\title{ Compute a new timeDate with a specific time on the same day}

\description{
Given a timeDate \code{DAY}, compute the beginning of that day, namely the time date with the same \code{year}, \code{month} and \code{day} slots, and 00:00:00 for the time (\code{hour}, \code{minute} and \code{second}), or the mid-point of the day with time 12:00:00.}

\usage{
begday(DAY), noon(DAY)
}

\arguments{
  \item{DAY}{ timeDate }
}
\value{
a timeDate with the same \code{year}, \code{month} and \code{day} slots}

\references{R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag, Chapter 5}
\author{Rene Carmona}


\seealso{}
\examples{
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
