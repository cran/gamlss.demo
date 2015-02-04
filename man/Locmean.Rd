\name{Locmean}

\alias{Locmean}
\alias{Locpoly}
\alias{WLocmean}
\alias{WLocpoly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to fit local regression
}
\description{ There are four function here to illustrate the fitting of local regressions.
i) \code{Locmean}, which uses local means within a symmetric local window,
ii) \code{Locpoly}, which uses a local polynomial fit within a symmetric local window.
iii) \code{WLocmean}, which uses a Gaussian kernel and
iv)  \code{WLocpoly}, which uses local polynomials weighted by a Gaussian kernel    
}
\usage{
Locmean(y, x = seq(1, length(y)), w = rep(1, length(y)), span = 0.5)
Locpoly(y, x = seq(1, length(y)), w = rep(1, length(y)), span = 0.5, order = 1)
WLocmean(y, x = seq(1, length(y)), w = rep(1, length(y)), lambda = 0.5)
WLocpoly(y, x = seq(1, length(y)), w = rep(1, length(y)), lambda = 0.5, order = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the response variable}
  \item{x}{the x-variable}
  \item{w}{prior weights}
  \item{span}{the side of the local window compare as a proportion to the total number of observations}
  \item{lambda}{the smoothing parameter for the Gaussian kernel}
  \item{order}{the order of the polynomial}
}
\details{
Those functions can be  used for illustration of the basic concepts of smoothing using small data sets. 
Do not use them with large data because are computationally inefficient. 
}
\value{The functions return a \code{locW} object with values 
  \item{fitted.values}{the fitted valus}
  \item{residuals}{the residuals}
  \item{edf}{the effective degrees of freedom}
  \item{rss}{the residual sum of squares}
  \item{lambda}{the smoothing parameter}
  \item{y}{the y variable}
  \item{x}{the x variable}
  \item{w}{the prior weights}  
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}) 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

}
\author{
Mikis Stasinopoulos, \email{d.stasinopoulos@londonmet.ac.uk}  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{loess}}, \code{\link{ksmooth}}
}
\examples{
library(MASS)
data(mcycle)
# local means
m0<-Locmean(mcycle$accel, mcycle$times, span=.1)
m1<-Locmean(mcycle$accel, mcycle$times, span=.2)
m2<-Locmean(mcycle$accel, mcycle$times, span=.3)
span <- c("span=0.1", "span=0.2", "span=0.3")
plot(accel~times, data=mcycle,main="local mean")
lines(fitted(m0)~mcycle$times, col=1, lty=1)
lines(fitted(m1)~mcycle$times, col=2, lty=2)
lines(fitted(m2)~mcycle$times, col=3, lty=3)
legend(1.5,50, legend = span, col = 1:3,
       lty = 1:3, cex = .8, y.intersp = 1)
#  kernel estimation      
k0<-WLocmean(mcycle$accel, mcycle$times, lambda=1)
k1<-WLocmean(mcycle$accel, mcycle$times,  lambda=2)
k2<-WLocmean(mcycle$accel, mcycle$times,  lambda=3)
lambda <- c("lambda=1", "lambda=2", "lambda=3")
plot(accel~times, data=mcycle,main="Gaussian kernel fit")
lines(fitted(k0)~mcycle$times, col=1, lty=1)
lines(fitted(k1)~mcycle$times, col=2, lty=2)
lines(fitted(k2)~mcycle$times, col=3, lty=3)
legend(1.5,50, legend = lambda, col = 1:3,
       lty = 1:3, cex = .8, y.intersp = 1)
# local polymials
l1<-Locpoly(mcycle$accel, mcycle$times, span=.1)
l2<-Locpoly(mcycle$accel, mcycle$times, span=.2)
l3<-Locpoly(mcycle$accel, mcycle$times, span=.3)

span <- c("span=0.1", "span=0.2", "span=0.3")
plot(accel~times, data=mcycle,main="local linear fit")
lines(fitted(l1)~mcycle$times, col=1, lty=1)
lines(fitted(l2)~mcycle$times, col=2, lty=2)
lines(fitted(l2)~mcycle$times, col=3, lty=3)
legend(1.5,50, legend = span, col = 1:3,
       lty = 1:3, cex = .8, y.intersp = 1)       
# weighted local polynomials  
lw1<-WLocpoly(mcycle$accel, mcycle$times, lambda=1.5, order=1)
lw2<-WLocpoly(mcycle$accel, mcycle$times, lambda=1.5, order=2)
lw3<-WLocpoly(mcycle$accel, mcycle$times, lambda=1.5, order=3)

span <- c("linear", "quadratic", "cubic")
plot(accel~times, data=mcycle,main="Weighted local linear, quadratic and cubic fits")
lines(fitted(lw1)~mcycle$times, col=1, lty=1)
lines(fitted(lw2)~mcycle$times, col=2, lty=2)
lines(fitted(lw3)~mcycle$times, col=3, lty=3)
legend(1.5,50, legend = span, col = 1:3,
       lty = 1:3, cex = .8, y.intersp = 1)     
}

\keyword{regression}

