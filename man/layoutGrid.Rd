\name{layoutGrid}
\alias{layoutGrid}
\title{Produce a layout of any number of nodes.}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\value{
This function returns the input data frame of nodes with the resulting layout applied.
}

\usage{
layoutGrid(N,string,name=NULL)
}
\arguments{
\item{N}{a data frame of nodes.}

\item{string}{a character vector specifing grouped nodes.}

\item{name}{name of column with node names.}
}
\description{
\code{layoutGrid} produces a grid layout of any number of nodes.
}
