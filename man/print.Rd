\name{print}
\alias{print.coin}
\alias{print.cooc}
\alias{print.netCoin}
\alias{print.barCoin}
\alias{print.timeCoin}
\alias{print}
\title{
Print a coin, netCoin, barCoin or timeCoin object.
}
\description{
Print n and a coincidence lower triangle matrix for \code{coin} and \code{cooc} objects.
Print title, nodes and links heads, and note for \code{netCoin} objects.
}
\usage{
print(x, ...)
}
\arguments{
  \item{x}{A coin object (see \code{\link{coin}}), netCoin object (see \code{\link{netCoin}}), barCoin object (see \code{\link{barCoin}}), timeCoin object (see \code{\link{timeCoin}}), cooc object (see \code{\link{coocur}})}
  \item{...}{Additional args}
}

\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}

\examples{
## Random incidence matrix: 25 scenarios, 4 events.
I <- matrix(rbinom(100,1,.5),nrow=25,ncol=4,
            dimnames=list(NULL,c("A","B","C","D")))

C <- coin(I,sort=TRUE)
C
print(C) #Alternatively

N <- allNet(I, sort=TRUE,main="Title",note="Comment")
N
print(N) #Alternatively
}
