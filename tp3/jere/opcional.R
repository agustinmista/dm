library(e1071)
source("stability.R")
source("gapStatistic.R")


wine <-  read.csv(file="C:/Users/Jeremías/Dropbox/FACU_2017/DM/tp3/wine.csv", header=F, sep=",")
names(wine) <- c("class","alcohol","malic acid","ash","alcalinity of ash","magnesium","total phenols","flavanoids","nonflavanoid phenols","proanthocyanins","color intensity","hue","OD280 of diluted wines","proline ")

#Preprocesamientos
lwine <- log(wine[,-c(1)])

#Log + pca
plwine <- data.frame(prcomp(lwine,retx=TRUE,scale=FALSE)$x)
#Log + scale
swine <- data.frame(scale(lwine,center=TRUE,scale=TRUE))
#Log + scale + pca
spwine <- data.frame(prcomp(lwine,retx=TRUE,scale=TRUE)$x)
#Log + pca + scale
pswine <- data.frame(scale(prcomp(lwine,retx=TRUE,scale=FALSE)$x))


#aplicación de los métodos
wine.kmeans     <- kmeans(wine[,-c(1)],3,50)$cluster
plwine.kmeans   <- kmeans(plwine,3,50)$cluster
swine.kmeans    <- kmeans(swine,3,50)$cluster
pswine.kmeans   <- kmeans(pswine,3,50)$cluster
spwine.kmeans   <- kmeans(spwine,3,50)$cluster

wine.hclustc     <- cutree(hclust(dist(wine[,-c(1)]),"complete"),k=3)
plwine.hclustc   <- cutree(hclust(dist(plwine),"complete"),k=3)
swine.hclustc    <- cutree(hclust(dist(swine),"complete"),k=3)
pswine.hclustc   <- cutree(hclust(dist(pswine),"complete"),k=3)
spwine.hclustc   <- cutree(hclust(dist(spwine),"complete"),k=3)

wine.hclusta     <- cutree(hclust(dist(wine[,-c(1)]),"average"),k=3)
plwine.hclusta   <- cutree(hclust(dist(plwine),"average"),k=3)
swine.hclusta    <- cutree(hclust(dist(swine),"average"),k=3)
pswine.hclusta   <- cutree(hclust(dist(pswine),"average"),k=3)
spwine.hclusta   <- cutree(hclust(dist(spwine),"average"),k=3)
wine.hclusts     <- cutree(hclust(dist(wine[,-c(1)]),"single"),k=3)
plwine.hclusts   <- cutree(hclust(dist(plwine),"single"),k=3)
swine.hclusts    <- cutree(hclust(dist(swine),"single"),k=3)
pswine.hclusts   <- cutree(hclust(dist(pswine),"single"),k=3)
spwine.hclusts   <- cutree(hclust(dist(spwine),"single"),k=3)



#Single method analysis
compare <- function(clust1,clust2){
  cont.table <-table(clust1,clust2)
  # Find optimal match between the two classifications
  class.match <- matchClasses(as.matrix(cont.table),method="exact")
  # Print the confusion table, with rows permuted to maximize the diagonal
  # print(cont.table[,class.match])
}

# ANÁLISIS DE LAS SOLUCIONES

clases <-wine[,1]
compare(wine.kmeans,clases)
compare(plwine.kmeans,clases)
compare(swine.kmeans,clases)
compare(pswine.kmeans,clases)
compare(spwine.kmeans,clases)

compare(wine.hclustc,clases)
compare(plwine.hclustc,clases)
compare(swine.hclustc,clases)
compare(pswine.hclustc,clases)
compare(spwine.hclustc,clases)

compare(wine.hclusta,clases)
compare(plwine.hclusta,clases)
compare(swine.hclusta,clases)
compare(pswine.hclusta,clases)
compare(spwine.hclusta,clases)

compare(wine.hclusts,clases)
compare(plwine.hclusts,clases)
compare(swine.hclusts,clases)
compare(pswine.hclusts,clases)
compare(spwine.hclusts,clases)


# ANÁLISIS DE K ÓPTIMO

gapWine <- function(N,method){
  res <- matrix(nrow=5,ncol=10)
  for(i in 1:N){
    res[1,i] <- gapStatistic(wine,5,5,method)
    res[2,i] <- gapStatistic(plwine,10,10,method)
    res[3,i] <- gapStatistic(swine,10,10,method)
    res[4,i] <- gapStatistic(pswine,10,10,method)
    res[5,i] <- gapStatistic(spwine,10,10,method)
  }
  return(res)
}

gsk <- gapWine(10,methodKmeans)
gshc <-gapWine(10,methodHclustC)
gsha <-gapWine(10,methodHclustA) 


s    <- stability(wine,5,15,mKmeans,0.9)
sps  <- stability(pswine,5,15,mKmeans,0.9)
ssp  <- stability(spwine,5,15,mKmeans,0.9)


