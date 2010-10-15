\name{demo.Locmean}
\alias{demo.Locmean}
\alias{demo.Locpoly}
\alias{demo.WLocmean}
\alias{demo.WLocpoly}


\title{ Demos for local polynomial smoothing} 

\description{ Those are four demos to show weighed and unweighed local mean and polynomial smoothing.} 


\usage{ 
demo.Locmean(y = NULL, x = NULL, ...) 
demo.Locpoly(y = NULL, x = NULL, ...)
demo.WLocpoly(y = NULL, x = NULL, ...)
demo.WLocmean(y = NULL, x = NULL, ...)
} 


\arguments{ 
  \item{y}{the response variable. If null it generates its own data }
  \item{x}{explanatory variable}
  \item{\dots}{for extra argument in the plot}
}

\value{It produces an rpanel plot }


\references{
Bowman, Bowman, Gibson and Crawford (2008) rpanel, CRAN

Eilers, P. H. C. and Marx, B. D. (1996). Flexible smoothing with
B-splines and penalties (with comments and rejoinder). \emph{Statist. Sci},
\bold{11}, 89-121.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,
   (with discussion), \emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}

\author{ Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}}



\seealso{\code{\link{demo.PenSplines}}}

\examples{
 demo.Locmean()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}
