\name{demo.BetaSplines}
\alias{demo.BetaSplines}
\alias{demo.discreteSmo}
\alias{demo.histSmo}
\alias{demo.interpolateSmo}
\alias{demo.PenSplines}

\title{Demos for smoothing techniques}
\description{These are demos for teaching smoothing techniques to students}

\usage{
demo.BetaSplines()
demo.discreteSmo(y = NULL, ...)
demo.histSmo(y = NULL, ...)
demo.interpolateSmo(y = NULL, w = NULL, ...)
demo.PenSplines(y = NULL, x = NULL, ...)
}

\arguments{
  \item{y}{for y variable if needed otherwise it is generated}
  \item{w}{for weights if needed}
  \item{x}{for explanatory variable if needed}
  \item{\dots}{for adding parameters in the plot}
}

\value{An rpanel plot}

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

\author{Paul Eirers \email{p.eilers@erasmusmc.nl}, Brian Marx \email{bmarx@LSU.EDU}, and Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}}

\examples{
demo.PenSplines()
}

\keyword{regression}
