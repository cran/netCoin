\name{dichotomize}
\alias{dichotomize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dichotomize.
}
\description{
This converts factor(s) o character(s) column(s) of a data frame into a set of dichotomous columns. Their names will correspond to the labels or text of every category.
}
\usage{
dichotomize(data, variables,
            sep = "", min = 1, length = 0, values = NULL,
            sparse = FALSE, add = TRUE, sort = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame with a factor or textual column which can be simple (only one value for each scenario) or multiple if components are delinited with a separator.}
  \item{variables}{vector of column names that have to be converted into dichotomous vectors.}
  \item{sep}{vector of characters used to divide columns with multiple events. If this separator is "", every unique cell of every column is converted into a dichotomus data frame's column.}
  \item{min}{convert to dichotomous vectors only label or text that has a frequency less or equal to the value of this parameter. If the value of min is between 0 and 1, its value is interpreted as a percentage}
  \item{length}{maximum number of dichotomous columns generated for every variable}
  \item{values}{vector of labels or texts selected to their conversion to dichotomous columns}
  \item{sparse}{produce a sparse matrix instead of a data.frame}
  \item{add}{add the new columns to the input data.frame}
  \item{sort}{order the new columns by their frequencies}
%%     ~~Describe \code{x} here~~
}

\value{
A data frame composed by the original plus the added dichotmous columns.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca, and Luis Martinez Uribe, Fundacion Juan March. See \url{http://casus.usal.es/blog/modesto-escobar/}
}
\examples{
# A character column
frame1 <- data.frame(A = c("Man", "Women", "Man", "Undet."))
dichotomize(frame1, "A", sep = "; ")

# A character column (with separator)
frame2 <- data.frame(A = c("Man; Women", "Women; Women",
                         "Man; Man", "Undet.; Women; Man"))
dichotomize(frame2, "A", sep = "; ")

# A character column and another factor column (same sepatator)
frame3 <- data.frame(A = c("Man; Women", "Women; Women",
                         "Man; Man", "Undet.; Women; Man"),
                     C = factor(c(1:4), labels = c("Paris", "New York",
                         "London; New York", "<NA>")))
dichotomize(frame3, c("A", "C"), sep = "; ")
}

