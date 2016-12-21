\name{network.addNodeAttr}
\alias{network.addNodeAttr}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
Return a network object.
}

\usage{
network.addNodeAttr(net, values, name="name", label = NULL, group = NULL,
                    size = NULL, color = NULL, shape = NULL, text = NULL)
}
\arguments{
\item{net}{a network object.}

\item{values}{a data frame including node names.}

\item{name}{name of the vector with names in the values data frame.}

\item{label}{name of the vector with labels in the values data frame.}

\item{group}{name of the vector with groups in the values data frame.}

\item{size}{name of the vector with size in the values data frame.}

\item{color}{name of the vector with color variable in the values data frame.}

\item{shape}{name of the vector with shape variable in the values data frame.}

\item{text}{name of the vector with html text in the values data frame.}
}
\description{
\code{network.addNodeAttr} add node attributes to an interactive network.
}
