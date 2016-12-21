\name{network.nodeImage}
\alias{network.nodeImage}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
Return a network object.
}

\usage{
network.nodeImage(net, image, name = "images")
}
\arguments{
\item{net}{a network object.}
\item{image}{a vector with the names of image files in the same order as in the nodes data frame.}
\item{name}{a vector giving the name to display in legend.}
}
\description{
\code{network.nodeImage} add images to an interactive network.
}
