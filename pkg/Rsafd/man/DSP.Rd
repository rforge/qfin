\name{DSP500}
\alias{DSP500}
\alias{DSP}
\alias{DSPLR}

\docType{data}
\title{Daily S&P 500 Index}
\description{
\item{DSP500}{Data frame whose columns \code{Date}, \code{Open}, \code{High}, \code{Low}, \code{Close} and \code{Volume}  give the date, the opening price, the lowest quote and the highest quote of the day, the closing price, and the volume}  
\item{DSP}{ Numeric vector giving the daily closing price of the S&P 500 index}
\item{DSPLR}{ Numeric vector giving the daily log-returns computed from the closing prices of \code{DSP}}
}
\usage{data(DSP500), data(DSP), data(DSPLR)}
\details{
\item{DSP500}{
        Date   Open    High    Low  Close   Volume
1  6-Jun-03 990.14 1007.69 986.01 987.76 18372000
2  5-Jun-03 986.24  990.14 978.13 990.14 16931000
3  4-Jun-03 971.56  987.85 970.72 986.24 16187000
4  3-Jun-03 967.00  973.02 964.47 971.56 14502000
5  2-Jun-03 963.59  979.11 963.59 967.00 16625000
6 30-May-03 949.64  965.38 949.64 963.59 16888000
}
\item{DSP}{
[1] 59.91 60.39 60.13 59.69 59.50 58.77
}
\item{DSPLR}{
[1]  0.007980092 -0.004314643 -0.007344383 -0.003188190 -0.012344792 -0.006144413
}
}
\references{
R. A. Carmona: Statistical Analysis of Financial Data in S-Plus, (2004) Springer Verlag
}
\examples{
data(DSP500), data(DSP), data(DSPLR)
}
\keyword{datasets}
