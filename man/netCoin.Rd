\name{netCoin}
\alias{netCoin}
\title{Networked coincidences.}
\description{
\code{netCoin} produces a netCoin object of coincidences. Its input has to be two data.frames: one of attributes of events or nodes, and the other of attributes of the edges or links. 
}
\value{
This function returns a \code{netCoin} object.
If the 'dir' attribute is specified, the function creates a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}

\usage{
netCoin(nodes, links, name = "name", label = NULL, size = NULL,
        color = NULL, shape = NULL, group = NULL, ntext = NULL,
        width = NULL, weight = NULL, lcolor = NULL, ltext = NULL,
        nodeFilter = NULL, linkFilter = NULL,
        main = NULL, note = NULL, help = NULL, layout = NULL,
        language = c("en","es"), image = NULL, dir = NULL, show = TRUE)
}
\arguments{
\item{nodes}{a data frame with at least one vector of names.}
\item{links}{a data frame with at least two vectors with source and target, including names of nodes.}
\item{name}{name of the vector with names in the nodes data frame.}
\item{label}{name of the vector with labels in the nodes data frame.}
\item{group}{name of the vector with groups in the nodes data frame.}
\item{size}{name of the vector with size in the nodes data frame.}
\item{color}{name of the vector with color variable in the nodes data frame.}
\item{shape}{name of the vector with shape variable in the nodes data frame.}
\item{ntext}{name of the vector with html text in the nodes data frame.}
\item{width}{name of the vector with width variable in the links data frame.}
\item{weight}{name of the vector with weight variable in the links data frame.}
\item{lcolor}{name of the vector with color variable in the links data frame.}
\item{ltext}{name of the vector with labels in the links data frame.}
\item{nodeFilter}{condition for filtering nodes.}
\item{linkFilter}{condition for filtering links.}
\item{main}{upper title of the graph.}
\item{note}{lower title of the graph.}
\item{help}{help text of the graph.}
\item{image}{name of the vector with image files in the nodes data frame.}
\item{layout}{a matrix with two columns.}
\item{language}{a character vector (es=spanish; en=english).}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
\item{show}{a logical value true if the graph is to be shown. Default = TRUE.}
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}
\examples{
# A character column (with separator)
frame <- data.frame(A = c("Man; Women", "Women; Women",
                        "Man; Man", "Undet.; Women; Man"))
data <- dichotomize(frame, "A", sep = "; ")[2:4]
C <- coin(data) # coincidence matrix
N <- asNodes(C) # node data frame
E <- edgeList(C) # edge data frame
netCoin(N, E) # netCoin object
}

