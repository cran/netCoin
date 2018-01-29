\name{surCoin}
\alias{surCoin}
\title{Networked coincidences from a data frame.}
\description{
\code{surCoin} produces a network object of coincidences from a data frame converting variables into dichotomies. 
}
\value{
This function creates a netCoin object (or igraph) and, if stated, a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}

\usage{
surCoin(data, variables=names(data), exogenous=NULL, commonlabel=NULL,
        weight=NULL, subsample=FALSE,
        minimum=1, maximum=nrow(data), sort=FALSE, decreasing=TRUE,
        nodes=NULL, frequency=FALSE, percentages=TRUE,
        name=NULL, label = NULL, ntext = NULL,
        size = "\%", color = NULL, shape = NULL, group = NULL, community = NULL,
        procedures="Haberman", criteria="Z", Bonferroni=FALSE,
        support=-Inf, minL=-Inf, maxL=Inf,
        directed=FALSE, diagonal=FALSE, sortL=NULL, decreasingL=TRUE,
        lwidth = NULL, lweight = NULL, lcolor = NULL, ltext = NULL,
        nodeFilter = NULL, linkFilter = NULL, degreeFilter = NULL,
        nodeBipolar = FALSE, linkBipolar = FALSE, defaultColor = "#1f77b4",
        main = NULL, note = NULL, help = NULL, background = NULL,
        layout = NULL, cex = 1, controls = c(1,2,3), mode = c("network","heatmap"),
        showCoordinates = FALSE, showArrows = FALSE, showLegend = TRUE,
        showAxes = FALSE, showLabels = TRUE, axesLabels = NULL,
        language = "en", image = NULL, dir = NULL, show = TRUE,
        igraph = FALSE)
}
\arguments{
  \item{data}{a data frame}
  \item{variables}{a vector of variables included in the previous data frame}
  \item{exogenous}{a vector of variables whose relations amongst them are of no interest. None by default}
  \item{commonlabel}{a vector of variables whose names are to be included in nodes labels}
  \item{weight}{a vector of weights. Optimal for data.framed tables}
  \item{subsample}{retrict the analysis to scenarios with at least one event}
  \item{minimum}{minimum frequency to be considered}
  \item{maximum}{maximum frequency to be considered}
  \item{sort}{sort the coincidence matrix according to frequency of events}
  \item{decreasing}{decreasing or increasing sort of the matrix}
  \item{nodes}{a data frame with at least one vector of names.}
  \item{frequency}{a logical value true if frequencies are to be shown. Default=FALSE.}
  \item{percentages}{a logical value true if percentages are to be shown. Default=TRUE.}
  \item{name}{name of the vector with names in the nodes data frame. By default, if language="en", name is "name", if language="es" is "nombre".}
  \item{label}{name of the vector with labels in the nodes data frame.}
  \item{ntext}{name of the vector with html text in the nodes data frame.}
  \item{size}{name of the vector with size in the nodes data frame.}
  \item{color}{name of the vector with color variable in the nodes data frame.}
  \item{shape}{name of the vector with shape variable in the nodes data frame.}
  \item{group}{name of the vector with groups in the nodes data frame.}
  \item{community}{algorithm to make communities: edge_betweenness("ed"), fast_greedy("fa"), label_prop("la"), leiden_eigen("le"), louvain("lo"), optimal("op"), spinglass("sp"), walktrap("wa")}
  \item{procedures}{a vector of statistics of similarity. See below.}
  \item{criteria}{statistic to be use for selection criteria.}
  \item{Bonferroni}{Bonferroni criterium of the signification test.}
  \item{support}{minimum value of the frequency of the coincidence to be edged}
  \item{minL}{minimum value of the statistic to include the edge in the list.}
  \item{maxL}{maximum value of the statistic to include the edge in the list.}
  \item{directed}{includes same edges only once.}
  \item{diagonal}{includes auto-links}
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
  \item{layout}{a matrix with two columns or an algorithm to elaborate the coordinates: davidson.harel
drl("da"), circle("ci"), fruchterman.reingold("fr"), gem("ge"), grid("gr"), kamada.kawai("ka"), lgl("lg"), mds("md"), random("ra"), reingold.tilfo("re"), star("sta"), sugiyama("sug")}
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
\item{show}{a logical value true if the graph is to be shown. Default = TRUE.}
\item{igraph}{Produces an igraph object instead of a netCoin object if TRUE}
}
\details{
Possible measures in procedures are 
\itemize{
\item{Frequencies (f), Relative frequencies (x), Conditional frequencies (i), Coincidence degree (cc), Probable degree (cp),}
\item{Expected (e), Confidence interval (con)}
\item{Matching (m), Rogers & Tanimoto (t), Gower (g), Sneath (s), Anderberg (and),}
\item Jaccard (j), Dice (d), antiDice (a), Ochiai (o), Kulczynski (k),
\item Hamann (ham), Yule (y), Pearson (p), odds ratio (od), Rusell (r), 
\item Haberman (h), Z value of Haberman (z), 
\item Hypergeometric p greater value (hyp).
\item Convert a matrix into an edge list (shape).
}
}

\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{http://casus.usal.es/blog/modesto-escobar/}
}
\examples{
# A data frame with two variables Gender and Opinion
frame<-data.frame(Gender=c(rep("Man",3),rep("Woman",3)),
                  Opinion=c("Yes","Yes","No","No","No","Yes"))
surCoin(frame,commonlabel="") # network object
}

