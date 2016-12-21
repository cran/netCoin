\name{print.coin}
\alias{print.coin}
\alias{print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a coin object.
}
\description{
Print n and a coincidence lower triangle matrix.
}
\usage{
print(x, ...)
}
\arguments{
  \item{x}{A coin object (see \code{\link{coin})}}
  \item{...}{Additional args}
}

\value{
A data frame
}

\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}

\examples{
## Random incidence matrix: 25 scenarios, 4 events.
I<-matrix(rbinom(100,1,.5),nrow=25,ncol=4,
          dimnames=list(NULL,c("A","B","C","D")))
coin(I,sort=TRUE)
I
print(I) #Alternatively
}
