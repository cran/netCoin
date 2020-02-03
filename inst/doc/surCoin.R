## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(netCoin)
data("ess")

## -----------------------------------------------------------------------------
head(ess)

## -----------------------------------------------------------------------------
set <- c("Social participation", "Political participation", "Gender", "Age")
essCoin <- surCoin(data = ess, variables = set)
essCoin

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  plot(essCoin)

## ----echo=FALSE, out.width='98%'----------------------------------------------
knitr::include_graphics("ess-a.png")

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  essCoin <- surCoin(data = ess,
#                      variables = set,
#                      dichotomies = c("Social participation", "Political participation"),
#                      valueDicho = "Yes"
#                      )
#  plot(essCoin)

## ----echo=FALSE, out.width='98%'----------------------------------------------
knitr::include_graphics("ess-b.png")

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  essCoin <- surCoin(data = ess,
#                      variables = set,
#                      dichotomies = c("Social participation", "Political participation"),
#                      valueDicho = "Yes",
#                      weight = "cweight",
#                      procedures = c("f", "i", "h"),
#                      )
#  plot(essCoin)

## ----echo=FALSE, out.width='98%'----------------------------------------------
knitr::include_graphics("ess-c.png")

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  essCoin <- surCoin(data = ess,
#                      variables = set,
#                      dichotomies = c("Social participation", "Political participation"),
#                      valueDicho = "Yes",
#                      weight = "cweight",
#                      procedures = c("f", "i", "h"),
#                      exogenous = c("Gender", "Age"),
#                      degreeFilter = 1,
#                      )
#  plot(essCoin)

## ----echo=FALSE, out.width='98%'----------------------------------------------
knitr::include_graphics("ess-d.png")

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  essCoin <- netCoin(essCoin,
#                      color = "variable",
#                      size = "%")
#  print(essCoin$nodes[1:5,], row.names=FALSE)
#  
#  plot(essCoin)

## ----eval=FALSE, echo=FALSE, out.width='98%'----------------------------------
#  knitr::include_graphics("ess-e.png")

## ----eval=FALSE, warning=FALSE------------------------------------------------
#  essCoin <- netCoin(essCoin,
#                      shape = "degree")
#  
#  plot(essCoin)

## ----echo=FALSE, out.width='98%'----------------------------------------------
knitr::include_graphics("ess-f.png")

## ----eval=FALSE---------------------------------------------------------------
#  essCoin <- netCoin(essCoin,
#                      dir = "./temp/ess/"
#                      )

## ----eval=FALSE---------------------------------------------------------------
#  essCoin.igraph <- toIgraph(essCoin)

