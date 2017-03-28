\name{fromIgraph}
\alias{fromIgraph}
\title{Produce interactive networks from igraph objects.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
This function returns a \code{netCoin} object.
If the 'dir' attribute is specified, the function creates a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}

\usage{
fromIgraph(G, layout = NULL, language = c("en","es"), dir = NULL)
}
\arguments{
\item{G}{an igraph object.}

\item{layout}{a matrix with two columns.}

\item{language}{a character vector (es=spanish; en=english).}

\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{fromIgraph} produce an interactive network from an igraph object.
}
