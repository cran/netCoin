\name{timeCoin}
\alias{timeCoin}
\title{Networked coincidences.}
\description{
\code{timeCoin} produces a timeline object. 
}
\value{
Object of class \code{timeline}.
}

\usage{
timeCoin(nodes, name = "name", start = "start", end = "end",
         group = NULL, text = NULL, main = NULL, minor = NULL,
         language = c("en","es"), dir = NULL)
}
\arguments{
\item{nodes}{a data frame with at least two vectors of names and incidences.}
\item{name}{name of the vector with names in the nodes data frame.}
\item{start}{name of the vector with starts in the nodes data frame.}
\item{end}{name of the vector with ends in the nodes data frame.}
\item{group}{name of the vector with groups in the nodes data frame.}
\item{text}{name of the vector with html text in the nodes data frame.}
\item{main}{upper title of the graph.}
\item{minor}{lower title of the graph.}
\item{language}{a character vector (es=spanish; en=english).}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}

