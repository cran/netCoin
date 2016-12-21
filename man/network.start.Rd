\name{network.start}
\alias{network.start}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
Return a network object.
}

\usage{
network.start(nodes, links, name = "name", main = NULL, minor = NULL, help = NULL)
}
\arguments{
\item{nodes}{a vector of node names.}

\item{links}{a data frame with two vectors of node names (source and target).}

\item{name}{name of the vector with names in the nodes data frame.}

\item{main}{upper title of the graph.}

\item{minor}{lower title of the graph.}

\item{help}{help text of the graph.}
}
\description{
\code{network.start} start an interactive network.
}
