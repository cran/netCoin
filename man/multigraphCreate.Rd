\name{multigraphCreate}
\alias{multigraphCreate}
\title{Produce interactive multi graphs.}
\author{
David Barrios and Carlos Prieto. Bioinformatics Service of Nucleus, University of Salamanca. See \url{http://bioinfo.usal.es/}
}
\value{
The function creates a folder in your computer with an HTML document named index.html which contains the graph. This file can be directly opened with your browser.
}

\usage{
multigraphCreate(..., language = c("en","es"), dir = "MultiGraph", show = TRUE)
}
\arguments{
\item{...}{coin graphs (netCoin, barCoin, timeCoin) objects or html "directories".}

\item{language}{a character vector.}

\item{dir}{a "character" string representing the directory where the graph will be saved.}
\item{show}{a logical value true if the graph is to be shown. Default = TRUE.}
}
\description{
\code{multigraphCreate} produce an interactive multi graph.
}
\examples{
# A character column (with separator)
frame <- data.frame(A = c("Man; Women", "Women; Women",
                        "Man; Man", "Undet.; Women; Man"))
data <- dichotomize(frame, "A", sep = "; ")[2:4]
C <- coin(data) # coincidence matrix
N <- asNodes(C) # node data frame
E <- edgeList(C,c("frequency","expected","haberman")) # edge data frame
bC<- barCoin(N,E) # barCoin object
cC<- cbarCoin(N,E) # barCoin object
nC<- netCoin(N,E) # netCoin object
multigraphCreate("Bar graph" = bC,
                  "Conditional bar graph" = cC, 
                  "Net graph"=nC, 
                  dir="./example", show = FALSE) # See ./example/index.html file
}
