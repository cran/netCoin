\name{network.addLinkAttr}
\alias{network.addLinkAttr}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
Return a network object.
}

\usage{
network.addLinkAttr(net, values, source="source", target = "target",
                    width = NULL, weight = NULL, color = NULL, text = NULL)
}
\arguments{
\item{net}{a network object.}

\item{values}{a data frame including source and target (node names).}

\item{source}{name of the vector with source names in the values data frame.}

\item{target}{name of the vector with target names in the values data frame.}

\item{width}{name of the vector with width variable in the values data frame.}

\item{weight}{name of the vector with weight variable in the values data frame.}

\item{color}{name of the vector with color variable in the values data frame.}

\item{text}{name of the vector with labels in the values data frame.}
}
\description{
\code{network.addLinkAttr} add link attributes to an interactive network.
}
