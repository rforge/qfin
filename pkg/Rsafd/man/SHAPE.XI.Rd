\name{SHAPE.XI}
\alias{SHAPE.XI}
\docType{data}

\title{PARAMETRIZATION OF GENERALIZED ETREME VALUES AND PARETO DISTRIBUTIONS}

\description{
      For financial applications, it is customary to
   define the GEV and GPD distributions as we did in formulae (\ref{equ:gev.finance}) and
 (\ref{equ:gpd.finance}) respectively. However, for other applications, e.g. hydrology,
 it is customary to use a parametrization based on the
 shape parameter $k = -\xi$. Under this alternative parametrization,
 the GEV cumulative function has the following form:
 \begin{equation}
    G(x) = \left\{
    \begin{array}{ll}
       \exp \left\{ - \left( 1 - \frac{k(x - m)}{\lambda} \right)^{1/k} \right\}
         &  k \ne 0,  \\
         \exp \left\{- \left( e^{-\frac{(x - m)}{\lambda}} \right) \right\}
        &  k = 0.
    \end{array} \right.
   \label{equ:gev.hydro}
   \end{equation}
   Here, $ -\infty < x \le m + \lambda/k$ for $k > 0$,
   $ -\infty < x < \infty$ for $k = 0$,
   and $m + \lambda/k  \le x < \infty$
   for $k < 0$.
   Similarly, the GDP cumulative distribution function becomes
 \begin{equation}
    F(x) = \left\{
    \begin{array}{ll}
     1 - \left( 1 - k \frac{x - m}{\lambda} \right)^{1/k} &  k \ne 0, \\
     1 - \exp \left\{ - \frac{x - m}{\lambda}  \right\} & k =
    0.
    \end{array} \right.
   \label{equ:gpd.hydro}
 \end{equation}
on the domains defined by $ m < x \le m + \lambda/k$ for $k > 0$,
   $m < x \le \infty$ for $k \le 0$.
\vskip 2pt\noindent
The user  may choose to work with the parametrization he or she is more comfortable with
 by setting the global variable \code{SHAPE.XI} to \code{TRUE} if parametrization given
 by formulae (\ref{equ:gev.finance} - \ref{equ:gpd.finance}) is preferred, 
 and to \code{FALSE} if the alternative form
 (\ref{equ:gev.hydro} - \ref{equ:gpd.hydro}) is desired.
}
      
\note{
    The values of most of the functions of the library \code{htd}
    are affected by the value of the boolean variable \texttt{SHAPE.XI}. Its default value is
    \texttt{TRUE}
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
