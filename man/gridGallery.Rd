\name{gridGallery}
\alias{gridGallery}
\title{Images in a grid gallery.}
\description{
\code{gridGallery} produces a gridGallery object. 
}
\value{
Object of class \code{gridGallery}.
}

\usage{
gridGallery(nodes, name = NULL, label = NULL, info = NULL, image = NULL,
            main = NULL, note = NULL, help = NULL,
            language = c("en", "es", "ca"), dir = NULL)
}
\arguments{
\item{nodes}{a data frame with at least three vectors of names, start and end.}
\item{name}{name of the vector with names in the nodes data frame.}
\item{label}{name of the vector with labels in the nodes data frame.}
\item{main}{upper title of the graph.}
\item{note}{lower title of the graph.}
\item{info}{name of the vector with information to display in a panel in the nodes data frame.}
\item{language}{a character vector (es=spanish; en=english; ca=catalan).}
\item{help}{help text of the graph.}
\item{image}{name of the vector with image files in the nodes data frame.}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}

\examples{
data("finches")
finches$species<-system.file("extdata", finches$species,
        package="netCoin") # copy path to the species field
gridGallery(finches, image="species", main="Species in Galapagos Islands",
        note="Data source: Sanderson (2000)")
}
