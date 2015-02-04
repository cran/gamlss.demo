\name{demo.LocalRegression}
\alias{demo.LocalRegression}
\alias{LPOL}
\alias{WLPOL}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Local Regression Smoothing}
\description{
This function demonstrate some characteristics of local regression Smoothing
}
\usage{
demo.LocalRegression(y = NULL, x = NULL, span = 0.5, 
                position = trunc((n - 1)/2), 
                deg = 1)
LPOL(y, x, span = 0.5, position = trunc((n - 1)/2), 
                w = rep(1, length(y)), deg = 1)
WLPOL(y, x, sd = 0.5, position = trunc((n - 1)/2), 
                w = rep(1, length(y)), deg = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{The response variable}
  \item{x}{the explanatory variable}
  \item{span}{The smoothing parameters}
  \item{sd}{The standard deviation of a normal kernel used as smoothing parameter}
  \item{position}{The position of the target values in the x axis}
  \item{w}{weights}
  \item{deg}{The degree of the local polynomial}
}
\details{
The function \code{demo.LocalRegression} demonstrates some aspects of the Local (unweighed) polynomial  regression. The functions \code{LPOL()} and \code{WLPOL()} produce plots related to unweighed and weighted local polynomial regression respectively. 
}
\value{
All function produce plots.
}
\references{
R Development Core Team (2010) tcltk package, CRAN. 

Bowman, Bowman, Gibson and Crawford (2008) rpanel, CRAN

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{
Mikis Stasinopoulos
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{demoDist}}, \code{\link{gamlss.demo}}
}
\examples{
demo.LocalRegression()
n <- 100
x <- seq(0, 1, length = n)*1.4
y <- 1.2 + .3*sin(5  * x) + rnorm(n) * 0.2
op <- par(mfrow=c(2,2))
LPOL(y,x, deg=0, position=5)
title("(a) moving average")
LPOL(y,x, deg=1,  position=75)
title("(b) linear poly")
WLPOL(y,x, deg=2, position=30)
title("(c) quadratic poly")
WLPOL(y,x, deg=3, position= 50)
title("(b) cubic poly")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}

