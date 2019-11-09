\name{fromIgraph}
\alias{fromIgraph}
\title{Produce interactive networks from igraph objects.}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\value{
This function returns a \code{netCoin} object.
If the 'dir' attribute is specified, the function creates a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}

\usage{
fromIgraph(G, ...)
}
\arguments{
\item{G}{an igraph object.}
\item{...}{Any \link{netCoin} argument.}
}
\description{
\code{fromIgraph} produce an interactive network from an igraph object.
}
