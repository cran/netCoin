## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3.6
)
library(netCoin)

## -----------------------------------------------------------------------------
head(iris, 3)

## -----------------------------------------------------------------------------
S <- surScat(iris, variables = names(iris), active = names(iris)[1:4])

## -----------------------------------------------------------------------------
S$options$axesLabels

## -----------------------------------------------------------------------------
head(S$nodes[, c("Species", "Groups(2)", "fx", "fy")], 3)

## -----------------------------------------------------------------------------
S

## -----------------------------------------------------------------------------
table(S$nodes[["Groups(2)"]], S$nodes$Species)

## ----eval = FALSE-------------------------------------------------------------
# plot(S)

## ----echo = FALSE-------------------------------------------------------------
op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), cex = 0.7)
for (v in c("Groups(2)", "Species")) {
  f <- factor(S$nodes[[v]])
  plot(S$nodes$fx, S$nodes$fy, col = seq_along(levels(f))[f], pch = 19, cex = 0.7,
       xlab = S$options$axesLabels[1], ylab = S$options$axesLabels[2], main = v)
  legend("topleft", legend = levels(f), col = seq_along(levels(f)), pch = 19, bty = "n")
}
par(op)

## -----------------------------------------------------------------------------
set.seed(1)
km <- kmeans(scale(iris[, 1:4]), 3, nstart = 25)
S <- addClusters(S, km, name = "kmeans(3)")
table(S$nodes[["kmeans(3)"]], S$nodes$Species)

## -----------------------------------------------------------------------------
hc <- cutree(hclust(dist(scale(iris[, 1:4]))), 3)
S <- addClusters(S, hc, name = "hclust(3)")
table(S$nodes[["hclust(3)"]], S$nodes$Species)

## ----eval = requireNamespace("tidyLPA", quietly = TRUE)-----------------------
mod <- tidyLPA::estimate_profiles(iris[, 1:4], 2:4,
                                  variances = "equal", covariances = "zero")
S <- addClusters(S, mod)
table(S$nodes[["LPA(3)"]], S$nodes$Species)

## -----------------------------------------------------------------------------
S <- addCommunities(S, iris, variables = names(iris)[1:4], community = c("lo", "wa"))
lou <- grep("^louvain", names(S$nodes), value = TRUE)
table(S$nodes[[lou]], S$nodes$Species)

## -----------------------------------------------------------------------------
levels(S$nodes[[lou]])

## ----error = TRUE-------------------------------------------------------------
try({
addCommunities(S, iris, variables = names(iris)[1:4], community = "op")
})

## -----------------------------------------------------------------------------
S <- addAxes(S, iris[, c("Petal.Length", "Sepal.Width")], name = "petals")
names(S$layouts)

## -----------------------------------------------------------------------------
S$clusters
S$clusters[["Groups(2).PCA"]]

## -----------------------------------------------------------------------------
S$clusters[["kmeans(3).petals"]]

## ----eval = FALSE-------------------------------------------------------------
# plot(S, dir = "./temp/iris/")   # write the interactive page to a directory
# saveGhml(S, file = "iris.graphml")

