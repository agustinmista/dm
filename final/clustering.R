library(MASS)
library(e1071)
library(cluster)

########################################
#               METHODS                #
########################################
kmeans.iters = 100

## km = kmeans; hc{s|c|a} = hclust {single|complete|average};
km  <- function(dataset) { kmeans(dataset, 2, kmeans.iters, nstart=10)$cluster }
hcs <- function(dataset) { cutree(hclust(dist(dataset), "single"), k=2) }
hcc <- function(dataset) { cutree(hclust(dist(dataset), "complete"), k=2) }
hca <- function(dataset) { cutree(hclust(dist(dataset), "average"), k=2) }

########################################
#              ANALYSIS                #
########################################

## Returns matching percentage between a class vector and a clustering.
clusterMatches <- function(class, clusters) {
  return(compareMatchedClasses(class, clusters, method="exact")$diag)
}

compareClusters <- function(c1, c2) {
  cont.table <- table(c1, c2)
  class.match <- matchClasses(as.matrix(cont.table), method="exact")
  print(cont.table[,class.match])
}

