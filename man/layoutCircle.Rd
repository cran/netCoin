\name{layoutCircle}
\alias{layoutCircle}
\title{Produce a circle layout of any number of nodes.}
\author{
David Barrios
}
\value{
This function returns the input data frame of nodes with the resulting layout applied.
}

\usage{
layoutCircle(N,nodes,deg=0,name=NULL)
}
\arguments{
\item{N}{a data frame of nodes.}

\item{nodes}{a vector specifing nodes.}

\item{deg}{degrees to rotate.}

\item{name}{name of column with node names.}
}
\description{
\code{layoutCircle} produces a circle layout of any number of nodes.
}
