\name{netCorr}
\alias{netCorr}
\title{Networked coincidences.}
\description{
\code{netCorr} produces a network object of correlations. Its input has to be at least one set of quantitative variables. 
}
\value{
The function creates a netCoin object and eventually a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\usage{
netCorr(variables, weight=NULL,
        minimum=-Inf, maximum=Inf, 
        sort=FALSE, decreasing=TRUE,
        nodes=NULL, frequency=FALSE, means=TRUE,
        name=NULL, label = NULL, ntext = NULL, 
        size = "mean", color = NULL, shape = NULL, 
        group = NULL, community = NULL, 
        method="pearson", criteria="p", Bonferroni=FALSE,
        minL=0, maxL=Inf,
        sortL=NULL, decreasingL=TRUE,
        lwidth = "value", lweight = "value", 
        lcolor = NULL, ltext = NULL,
        nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL,
        nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
        main = NULL, note = NULL, help = NULL, background = NULL,
        layout = NULL, cex = 1, controls = c(1,2,3), mode = c("network","heatmap"),
        showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE, 
        showAxes = FALSE, showLabels = TRUE,                   
        axesLabels = NULL, language = "en", 
        image = NULL,  dir = NULL, show = TRUE,
        igraph=FALSE 
        )
}
\arguments{
\item{variables}{a data frame with at least two quantitative variables.}
\item{weight}{a vector of weights. Optimal for data.framed tables}
\item{minimum}{minimum frequency to be considered}
\item{maximum}{maximum frequency to be considered}
\item{sort}{sort the correlation matrix according to the frequency of the events}
\item{decreasing}{decreasing or increasing sort of the matrix}
\item{nodes}{a data frame with at least one vector of names.}
\item{frequency}{a logical value true if frequencies are to be shown. Default=FALSE.}
\item{means}{a logical value true if means are to be shown. Default=TRUE.}
\item{name}{name of the vector with names in the nodes data frame. By default, if language="en", name is "name", if language="es" is "nombre".}
\item{label}{name of the vector with labels in the nodes data frame.}
\item{ntext}{name of the vector with html text in the nodes data frame.}
\item{size}{name of the vector with size in the nodes data frame.}
\item{color}{name of the vector with color variable in the nodes data frame.}
\item{shape}{name of the vector with shape variable in the nodes data frame.}
\item{group}{name of the vector with groups in the nodes data frame.}
\item{community}{algorithm to make communities: edge_betweenness("ed"), fast_greedy("fa"), label_prop("la"), leiden_eigen("le"), louvain("lo"), optimal("op"), spinglass("sp"), walktrap("wa")}
\item{method}{a vector of statistics of similarity. Pearson correlation by default. spearman and kendall are also possible}
\item{criteria}{statistic to be use for selection criteria.}
\item{Bonferroni}{Bonferroni criterium of the signification test.}
\item{minL}{minimum value of the statistic to include the edge in the list.}
\item{maxL}{maximum value of the statistic to include the edge in the list.}
\item{sortL}{sort the list according to the values of a statistic. See below}
\item{decreasingL}{order in a decreasing way.}
\item{lwidth}{name of the vector with width variable in the links data frame.}
\item{lweight}{name of the vector with weight variable in the links data frame.}
\item{lcolor}{name of the vector with color variable in the links data frame.}
\item{ltext}{name of the vector with labels in the links data frame.}
\item{nodeFilter}{condition for filtering nodes.}
\item{linkFilter}{condition for filtering links.}
\item{degreeFilter}{condition to filter the resulting network by degree.}
\item{nodeBipolar}{a logical value that polarizes negative and positive node values in the graphical representation. Default = FALSE.}
\item{linkBipolar}{a logical value that polarizes negative and positive link values in the graphical representation. Default = FALSE.}
\item{defaultColor}{a character vector giving a valid html color.}
\item{main}{upper title of the graph.}
\item{note}{lower title of the graph.}
\item{help}{help text of the graph.}
\item{background}{background color or image of the graph.}
\item{layout}{a matrix with two columns.}
\item{cex}{number indicating the amount by which plotting text should be scaled relative to the default. Default = 1.}
\item{controls}{a numeric vector indicating which controls will be shown. 1 = sidebar, 2 = selection buttons, 3 = tables.}
\item{mode}{a character vector indicating the graph mode allowed: network, heatmap or both (both by default).}
\item{showCoordinates}{a logical value true if the coordinates are to be shown in tables. Default = FALSE.}
\item{showArrows}{a logical value true if the directional arrows are to be shown. Default = FALSE.}
\item{showLegend}{a logical value true if the legend is to be shown. Default = TRUE.}
\item{showAxes}{a logical value true if the axes are to be shown. Default = FALSE.}
\item{showLabels}{a logical value true if the node labels are to be shown. Default = TRUE.}
\item{axesLabels}{a character vector giving the axes names.}
\item{language}{a character vector (es=spanish; en=english).}
\item{image}{name of the vector with image files in the nodes data frame.}
\item{dir}{a "character" string representing the directory where the web files will be saved.}
\item{igraph}{Produces an igraph object instead of a netCoin object if TRUE}
\item{show}{a logical value true if the graph is to be shown. Default = TRUE.}
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}
\examples{
# A character column (with separator)
data(iris)
netCorr(iris[,1:4],ltext="value",
 main="Correlations between measurements of Iris Species",
 note="Anderson, Edgar (1935) y Fisher, R. A. (1936)") # network object
}

