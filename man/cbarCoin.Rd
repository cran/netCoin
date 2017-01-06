\name{cbarCoin}
\alias{cbarCoin}
\title{Networked coincidences.}
\description{
\code{cbarCoin} produces a barplot object. 
}
\value{
Object of class \code{barplot}.
}

\usage{
cbarCoin(nodes, links, name = "name", label = NULL, text = NULL,
         incidences = "frequency", coincidences = "coincidences",
         expected = "expected", confidence.interval = NULL,
         minor = NULL, language = c("en","es"), dir = NULL)
}
\arguments{
\item{nodes}{a data frame with at least two vectors of names and incidences.}
\item{links}{a data frame with at least three vectors with source and target, including names of nodes and a vector of coincidences.}
\item{name}{name of the vector with names in the nodes data frame.}
\item{label}{name of the vector with labels in the nodes data frame.}
\item{text}{name of the vector with html text in the nodes data frame.}
\item{coincidences}{name of the vector with coincidences in the links data frame.}
\item{incidences}{name of the vector with incidences in the nodes data frame.}
\item{expected}{name of the vector with expected coincidences in the links data frame.}
\item{confidence.interval}{name of the vector with confidence interval in the links data frame.}
\item{minor}{lower title of the graph.}
\item{language}{a character vector (es=spanish; en=english).}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
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
N <- as.nodes(C) # node data frame
E <- edgeList(C,"frequency","expected") # edge data frame
cbarCoin(N,E) # barplot object
}
