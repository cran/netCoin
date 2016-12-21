\name{network.all}
\alias{network.all}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
The function creates a folder in your computer with an HTML document named index.html which contains the graph. This file can be directly opened with your browser.
}

\usage{
network.all(nodes, links, name = "name", source = "source", target = "target",
            layout = NULL, language = c("en","es"), dir = "Network")
}
\arguments{
\item{nodes}{a data frame with at least one vector of names.}

\item{links}{a data frame with at least two vectors: source and target (node names).}

\item{name}{name of the vector with names in the nodes data frame.}

\item{source}{name of the vector with source names in the links data frame.}

\item{target}{name of the vector with target names in the links data frame.}

\item{layout}{a matrix with two columns.}

\item{language}{a character vector (es=spanish; en=english).}

\item{dir}{a "character" string representing the directory where the web files will be saved.}
}
\description{
\code{network.all} produce an interactive network.
}
