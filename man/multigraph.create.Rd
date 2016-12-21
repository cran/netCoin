\name{multigraph.create}
\alias{multigraph.create}
\alias{multinet.create}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
The function creates a folder in your computer with an HTML document named index.html which contains the graph. This file can be directly opened with your browser.
}

\usage{
multigraph.create(..., language = c("en","es"), dir = "MultiGraph")
multinet.create(..., language = c("en","es"), dir = "MultiGraph")
}
\arguments{
\item{...}{network objects.}

\item{language}{a character vector.}

\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{multigraph.create} produce an interactive multi network.
}
