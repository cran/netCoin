\name{as.nodes}
\alias{as.nodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nodes data frame.}
\description{Nodes data frame from either an edge list or a \code{coin} object.}
\usage{
as.nodes(C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C}{has to be an edge list or, better, a \code{coin} object.}
}
\value{A data frame with nodes' names and their frequency if the input is a \code{coin} object}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}
\examples{
# From a random incidence matrix I(25X4)
I <- matrix(rbinom(100, 1, .5), nrow = 25, ncol = 4,
     dimnames = list(NULL, c("A", "B", "C", "D")))
C <- coin(I)
as.nodes(C)
}
