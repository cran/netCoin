\name{network.create}
\alias{network.create}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
The function creates a folder on your computer with an HTML document named index.html which contains the graph. This file can be directly opened with your browser.
}

\usage{
network.create(net, language = c("en","es"), dir = "Network")
}
\arguments{
\item{net}{a network object.}

\item{language}{a character vector (es=spanish; en=english).}

\item{dir}{a "character" string representing the directory where the web files will be saved.}
}
\description{
\code{network.create} produce an interactive network.
}
