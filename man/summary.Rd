\name{summary}
\alias{summary.coin}
\alias{summary.netCoin}
\alias{summary.barCoin}
\alias{summary.timeCoin}
\alias{summary}
\title{
Summary of a coin or netCoin object.
}
\description{
Produce result summaries of coin or netCoin objects.
}
\usage{
summary(object, ...)
}
\arguments{
  \item{object}{A coin object (see \code{\link{coin}}), netCoin object (see \code{\link{netCoin}}), barCoin object (see \code{\link{barCoin}}), timeCoin object (see \code{\link{timeCoin}})}
  \item{...}{Additional args}
}

\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}

\examples{
## Random incidence matrix: 25 scenarios, 4 events.
I <- matrix(rbinom(100,1,.5),nrow=25,ncol=4,
            dimnames=list(NULL,c("A","B","C","D")))

N <- allNet(I, sort=TRUE,main="Title",note="Comment")
summary(N)
}
