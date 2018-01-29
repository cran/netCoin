\name{plot}
\alias{plot.coin}
\alias{plot.netCoin}
\alias{plot.barCoin}
\alias{plot.timeCoin}
\alias{plot}
\title{
Plot a coin, netCoin, barCoin or timeCoin object.
}
\description{
Function for plotting of R objects.
}
\usage{
plot(x, ...)
}
\arguments{
  \item{x}{A coin object (see \code{\link{coin}}), netCoin object (see \code{\link{netCoin}}), barCoin object (see \code{\link{barCoin}}), timeCoin object (see \code{\link{timeCoin}})}
  \item{...}{Additional args}
}

\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}

\examples{
## Random incidence matrix: 25 scenarios, 4 events.
I <- matrix(rbinom(100,1,.5),nrow=25,ncol=4,
            dimnames=list(NULL,c("A","B","C","D")))

N <- allNet(I, sort=TRUE,main="Title",note="Comment")
plot(N)
}
