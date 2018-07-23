\name{sim}
\alias{sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity matrix.
}
\description{
It calculates a similarity/distance matrix from either an incidence data frame/matrix or a coin object.
}
\usage{
sim(input, procedures="Jaccard", distance=FALSE, minimum=1, maximum=Inf, 
    sort=FALSE, decreasing=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{a binary data frame or a coin object, let's say an R list composed by a number of scenarios (\code{$n}) and a coincidence matrix with frequencies (\code{$f}).}
  \item{procedures}{a vector of statistics of similarity. See details below.}
  \item{distance}{convert the similarity matrix into a distance matrix}
  \item{minimum}{minimum frequency to obtain a similarity/distance measure.}
  \item{maximum}{maxium frequency to obtain a similarity/distance measure.}
  \item{sort}{sort the list according to the values of a statistic. See details below}
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
\item Haberman (h), Z value of Haberman (z).
\item Hypergeometric p greater value (hyp).
}
}
\value{
A similarity/distance matrix.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
# From a random incidence matrix I(25X4)
I<-matrix(rbinom(100,1,.5),nrow=25,ncol=4,
   dimnames=list(NULL,c("A","B","C","D")))
sim(I)
#Same results
C<-coin(I)
sim(C)

}
