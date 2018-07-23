\name{edgeList}
\alias{edgeList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Edge list.
}
\description{
Convert a coincidence/similarity/distance matrix into an edge list form.
}
\usage{
edgeList(data, procedures="Haberman", criteria="Z", Bonferroni=FALSE, 
         min=-Inf, max=Inf, support=-Inf,
         directed=FALSE, diagonal= FALSE, sort=NULL, decreasing=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a coin object, let's say an R list composed by a number of scenarios (\code{$n}) and a coincidence matrix with frequencies (\code{$f}). In case of change of shape, \code{data} should be a matrix.}
  \item{procedures}{a vector of statistics of similarity. See below.}
  \item{criteria}{statistic to be use for selection criteria.}
  \item{Bonferroni}{Bonferroni criterium of the signification test.}
  \item{min}{minimum value of the statistic to include the edge in the list.}
  \item{max}{maximum value of the statistic to include the edge in the list.}
  \item{support}{minimum value of the frequency of the coincidence to be edged}
  \item{directed}{includes same edges only once.}
  \item{diagonal}{includes auto-links}
  \item{sort}{sort the list according to the values of a statistic. See below}
  \item{decreasing}{order in a decreasing way.}
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
\value{
A data frame in which the two first columns are source and target. The rest of the columns are the different statistics explicited in \code{funcs} parameter.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
# From a random incidence matrix I(25X4)
I<-matrix(rbinom(100,1,.5),nrow=25,ncol=4,
dimnames=list(NULL,c("A","B","C","D")))
C<-coin(I)
edgeList(C)
}

