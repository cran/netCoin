\name{network.fromIgraph}
\alias{network.fromIgraph}
\title{Produce interactive network from igraph object.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
This function creates a folder on your computer with an HTML document named index.html which contains the graph. This file can be directly opened with your browser.
}

\usage{
network.fromIgraph(G, layout = NULL, language = c("en","es"), dir = "Network")
}
\arguments{
\item{G}{an igraph object.}

\item{layout}{a matrix with two columns.}

\item{language}{a character vector (es=spanish; en=english).}

\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{network.fromIgraph} produce an interactive network from igraph.
}
