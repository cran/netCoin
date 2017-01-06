\name{multigraph.create}
\alias{multigraph.create}
\title{Produce interactive network.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
The function creates a folder in your computer with an HTML document named index.html which contains the graph. This file can be directly opened with your browser.
}

\usage{
multigraph.create(..., language = c("en","es"), dir = "MultiGraph")
}
\arguments{
\item{...}{network objects.}

\item{language}{a character vector.}

\item{dir}{a "character" string representing the directory where the graph will be saved.}
}
\description{
\code{multigraph.create} produce an interactive multi network.
}
\examples{
# A character column (with separator)
frame <- data.frame(A = c("Man; Women", "Women; Women",
                        "Man; Man", "Undet.; Women; Man"))
data <- dichotomize(frame, "A", sep = "; ")[2:4]
C <- coin(data) # coincidence matrix
N <- as.nodes(C) # node data frame
E <- edgeList(C,c("frequency","expected","haberman")) # edge data frame
bC<- barCoin(N,E) # barplot object
cC<- cbarCoin(N,E) # barplot object
nC<- netCoin(N,E) # network object
multigraph.create("Bar graph"=bC,
                  "Conditional bar graph"=cC, 
                  "Net graph"=nC, 
                  dir="./example") # See ./example/index.html file
}
