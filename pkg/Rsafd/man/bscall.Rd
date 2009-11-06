\name{bscall}
\alias{bscall}
\title{Black-Scholes Price of an European Call Option}
\description{
Evaluate the Black-Scholes formula for the price of an European call option
}
\usage{
bscall(TAU = 0.03, K = 1, S, R = 0.1, SIG = 0.15)
}

\arguments{
  \item{TAU}{Time to maturity (in years)}
  \item{K}{Strike of the option}
  \item{S}{Current value of the underlier of the option }
  \item{R}{Yearly short interest rate}
  \item{SIG}{Volatility of the underlier}
}

\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{Rene Carmona}
\seealso{\code{\link{isig}}}
\examples{
}