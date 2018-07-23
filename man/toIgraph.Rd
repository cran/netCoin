\name{toIgraph}
\alias{toIgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{igraph object.}
\description{\code{igraph} object from a \code{netCoin} object.}
\usage{
toIgraph(net)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{is a \code{netCoin} object. See \code{\link{netCoin}}}
}
\value{An \code{igraph} object.}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
# A character column (with separator)
frame <- data.frame(A = c("Man; Women", "Women; Women",
                        "Man; Man", "Undet.; Women; Man"))
data <- dichotomize(frame, "A", sep="; ")[2:4]
C <- coin(data) # coincidence matrix
N <- asNodes(C) # node data frame
E <- edgeList(C) # edge data frame
net <- netCoin(N, E) # netCoin object
toIgraph(net) # conversion into a igraph object
}
