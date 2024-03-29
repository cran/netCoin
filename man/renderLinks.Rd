\name{renderLinks}
\alias{renderLinks}
\title{Create an html list of links.}
\usage{
renderLinks(data, columns, target = "_blank", sites = NULL)
}
\arguments{
\item{data}{data frame which contains the data.}
\item{columns}{column name which contains the urls.}
\item{target}{The target attribute specifies where to open the linked document: '_blank' opens the linked document in a new window or tab; '_self' opens the linked document in the same frame as it was clicked; \emph{framename} opens the linked document in the named iframe.}
\item{sites}{A data frame of 3 columns (url, name, icon) with the sites that the function will recognize.}
}
\value{
a character vector of html formatted links.
}
\description{
Create an html list of links.
}
\examples{
links <- data.frame(name = c(
    "Camarhynchus psitticula",
    "Camarhynchus pauper",
    "Camarhynchus parvulus"
  ), wikipedia=c(
    "https://en.wikipedia.org/wiki/Large_tree_finch",
    "https://en.wikipedia.org/wiki/Medium_tree_finch",
    "https://en.wikipedia.org/wiki/Small_tree_finch"
  ),wikidata=c(
    "https://www.wikidata.org/wiki/Q578835",
    "https://www.wikidata.org/wiki/Q1125857",
    "https://www.wikidata.org/wiki/Q1086136"
  ))
html <- renderLinks(links,c("wikipedia","wikidata"))
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}

