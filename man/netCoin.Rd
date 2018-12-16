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
netCoin(nodes, links = NULL, tree = NULL, name = NULL, label = NULL, size = NULL,
        color = NULL, shape = NULL, legend = NULL, ntext = NULL, info = NULL,
        orderA = NULL, orderD = NULL, group = NULL, community = NULL,
        lwidth = NULL, lweight = NULL, lcolor = NULL, ltext = NULL,
        nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL,
        nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
        main = NULL, note = NULL, help = NULL, helpOn = FALSE, cex = 1,
        background = NULL, layout = NULL, controls = c(1,2,3),
        mode = c("network","heatmap"), showCoordinates = FALSE,
        showArrows = FALSE, showLegend = TRUE, showAxes = FALSE,
        axesLabels = NULL, language = c("en","es"),
        image = NULL, imageNames = NULL, dir = NULL, show = TRUE)
}
\arguments{
\item{nodes}{a data frame with at least one vector of names.}
\item{links}{a data frame with at least two vectors with source and target, including names of nodes.}
\item{tree}{a data frame with two vectors: source and target, describing relationships between nodes.}
\item{name}{name of the vector with names in the nodes data frame. By default, if language="en", name is "name", if language="es" is "nombre".}
\item{label}{name of the vector with labels in the nodes data frame.}
\item{group}{name of the vector with groups in the nodes data frame.}
\item{community}{algorithm to make communities: edge_betweenness("ed"), fast_greedy("fa"), label_prop("la"), leiden_eigen("le"), louvain("lo"), optimal("op"), spinglass("sp"), walktrap("wa")}
\item{size}{name of the vector with size in the nodes data frame.}
\item{color}{name of the vector with color variable in the nodes data frame.}
\item{shape}{name of the vector with shape variable in the nodes data frame.}
\item{legend}{name of the vector with the variable to represent as a legend in the nodes data frame.}
\item{ntext}{name of the vector with html text in the nodes data frame.}
\item{info}{name of the vector with information to display in a panel in the nodes data frame.}
\item{orderA}{name of the vector with node ascending order in the nodes data frame.}
\item{orderD}{name of the vector with node descending order in the nodes data frame.}
\item{lwidth}{name of the vector with width variable in the links data frame.}
\item{lweight}{name of the vector with weight variable in the links data frame.}
\item{lcolor}{name of the vector with color variable in the links data frame.}
\item{ltext}{name of the vector with labels in the links data frame.}
\item{nodeFilter}{condition for filtering nodes.}
\item{linkFilter}{condition for filtering links.}
\item{degreeFilter}{numeric vector to filter the resulting network by degree.}
\item{nodeBipolar}{a logical value that polarizes negative and positive node values in the graphical representation. Default = FALSE.}
\item{linkBipolar}{a logical value that polarizes negative and positive link values in the graphical representation. Default = FALSE.}
\item{defaultColor}{a character vector giving a valid html color.}
\item{main}{upper title of the graph.}
\item{note}{lower title of the graph.}
\item{help}{help text of the graph.}
\item{helpOn}{Should the help be shown at the beginning?}
\item{background}{background color or image of the graph.}
\item{layout}{a matrix with two columns or an algorithm to elaborate the coordinates: davidson.harel
drl("da"), circle("ci"), fruchterman.reingold("fr"), gem("ge"), grid("gr"), kamada.kawai("ka"), lgl("lg"), mds("md"), random("ra"), reingold.tilfo("re"), star("sta"), sugiyama("sug")}
\item{cex}{number indicating the amount by which plotting text should be scaled relative to the default. Default = 1.}
\item{controls}{a numeric vector indicating which controls will be shown. 1 = sidebar, 2 = selection buttons, 3 = tables.}
\item{mode}{a character vector indicating the graph mode allowed: network, heatmap or both (both by default).}
\item{showCoordinates}{a logical value true if the coordinates are to be shown in tables. Default = FALSE.}
\item{showArrows}{a logical value true if the directional arrows are to be shown. Default = FALSE.}
\item{showLegend}{a logical value true if the legend is to be shown. Default = TRUE.}
\item{showAxes}{a logical value true if the axes are to be shown. Default = FALSE.}
\item{axesLabels}{a character vector giving the axes names.}
\item{language}{a character vector (es=spanish; en=english).}
\item{image}{name of the vector with image files in the nodes data frame.}
\item{imageNames}{name of the vector with names for image files in the nodes data frame.}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
\item{show}{a logical value true if the graph is to be shown. Default = TRUE.}
}
\note{
nodes and links arguments can be substituted by a netCoin object to add or change options to it.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
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

