\name{timeCoin}
\alias{timeCoin}
\title{Networked coincidences.}
\description{
\code{timeCoin} produces a timeCoin object. 
}
\value{
Object of class \code{timeCoin}.
}

\usage{
timeCoin(nodes, name = "name", start = "start", end = "end",
         group = NULL, text = NULL, main = NULL, note = NULL, cex = 1,
         language = c("en","es"), dir = NULL, show = TRUE)
}
\arguments{
\item{nodes}{a data frame with at least two vectors of names and incidences.}
\item{name}{name of the vector with names in the nodes data frame.}
\item{start}{name of the vector with starts in the nodes data frame.}
\item{end}{name of the vector with ends in the nodes data frame.}
\item{group}{name of the vector with groups in the nodes data frame.}
\item{text}{name of the vector with html text in the nodes data frame.}
\item{main}{upper title of the graph.}
\item{note}{lower title of the graph.}
\item{cex}{number indicating the amount by which plotting text should be scaled relative to the default. Default = 1.}
\item{language}{a character vector (es=spanish; en=english).}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
\item{show}{a logical value true if the graph is to be shown. Default = TRUE.}
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}

\examples{
# Database of 19th century sociologists
data(sociologists)
timeCoin(sociologists,"name","birth","death","birthcountry", 
         dir = "./timeline", show = FALSE) # See ./timeline/index.html file
}
