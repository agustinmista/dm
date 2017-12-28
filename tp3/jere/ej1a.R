# DATASET CRABS
library(MASS)
data(crabs)
library(e1071)

classes <- crabs[,1:2]

# Preprocesamiento
#Log
lcrabs <- log(crabs[,4:8])
#Log + scale
scrabs <- data.frame(scale(lcrabs,center=TRUE,scale=TRUE))
#Log + scale + pca
pcrabs <- data.frame(prcomp(lcrabs,retx=TRUE,scale=TRUE)$x)
#Log + pca
pcrabs2 <- data.frame(prcomp(lcrabs,retx=TRUE,scale=FALSE)$x)
#Log + pca + scale
pcrabs3 <- data.frame(scale(prcomp(lcrabs,retx=TRUE,scale=FALSE)$x))
                                    
                                                  
# Algunos plots de los conjuntos procesados

#plot(crabs[4:8],col=classes[,1])
#plot(lcrabs,col=classes[,1])
#plot(scrabs,col=classes[,1])
#plot(pcrabs,col=classes[,1])

# Aplico kmeans y hclust a todo

crabs.kmeans   <- kmeans(crabs[4:8],2,50)$cluster
lcrabs.kmeans  <- kmeans(lcrabs,2,50)$cluster
scrabs.kmeans  <- kmeans(scrabs,2,50)$cluster
pcrabs.kmeans  <- kmeans(pcrabs,2,50)$cluster
pcrabs2.kmeans <- kmeans(pcrabs2,2,50)$cluster
pcrabs3.kmeans <- kmeans(pcrabs3,2,50)$cluster

crabs.hclusts   <- cutree(hclust(dist(crabs[4:8]),"single"),k=2)
lcrabs.hclusts  <- cutree(hclust(dist(lcrabs),"single"),k=2)
scrabs.hclusts  <- cutree(hclust(dist(scrabs),"single"),k=2)
pcrabs.hclusts  <- cutree(hclust(dist(pcrabs),"single"),k=2)
pcrabs2.hclusts <- cutree(hclust(dist(pcrabs2),"single"),k=2)
pcrabs3.hclusts <- cutree(hclust(dist(pcrabs3),"single"),k=2)

crabs.hclustc   <- cutree(hclust(dist(crabs[4:8]),"complete"),k=2)
lcrabs.hclustc  <- cutree(hclust(dist(lcrabs),"complete"),k=2)
scrabs.hclustc  <- cutree(hclust(dist(scrabs),"complete"),k=2)
pcrabs.hclustc  <- cutree(hclust(dist(pcrabs),"complete"),k=2)
pcrabs2.hclustc <- cutree(hclust(dist(pcrabs2),"complete"),k=2)
pcrabs3.hclustc <- cutree(hclust(dist(pcrabs3),"complete"),k=2)

crabs.hclusta   <- cutree(hclust(dist(crabs[4:8]),"average"),k=2)
lcrabs.hclusta  <- cutree(hclust(dist(lcrabs),"average"),k=2)
scrabs.hclusta  <- cutree(hclust(dist(scrabs),"average"),k=2)
pcrabs.hclusta  <- cutree(hclust(dist(pcrabs),"average"),k=2)
pcrabs2.hclusta <- cutree(hclust(dist(pcrabs2),"average"),k=2)
pcrabs3.hclusta <- cutree(hclust(dist(pcrabs3),"average"),k=2)

#Single method analysis

compare <- function(clust1,clust2){
  cont.table <-table(clust1,clust2)
  # Find optimal match between the two classifications
  class.match <- matchClasses(as.matrix(cont.table),method="exact")
  # Print the confusion table, with rows permuted to maximize the diagonal
 # print(cont.table[,class.match])
}
# ANÁLISIS DE CLUSTERS

#nos va a decir cuantos de cada clase hay en cada cluster
analisis <- function(clases,clusters){
  cA <- levels(clases)[1]
  cB <- levels(clases)[2]
  idcA <- which(clases==cA)
  idcB <- which(clases==cB)
  clusters<-clusters-1  # Tenemos dos clusters: 0 y 1
  
  cAc1 <- sum(clusters[idcA])   # Cuantos elementos de clase A estan en el cluster 1
  cAc0 <- length(idcA) - cAc1 
  cBc1 <- sum(clusters[idcB])   # Cuantos elementos de clase B estan en el cluster 1
  cBc0 <- length(idcB) - cBc1
  
  return(cbind(cAc1,cAc0,cBc1,cBc0))
}

#Compara con todos los clusters
analizartodo_kmeans <- function(clases){
  return( rbind(
    analisis(clases,crabs.kmeans),
    analisis(clases,lcrabs.kmeans),
    analisis(clases,scrabs.kmeans),
    analisis(clases,pcrabs.kmeans),
    analisis(clases,pcrabs2.kmeans),
    analisis(clases,pcrabs3.kmeans)
  ) )
}
analizartodo_hclusts <- function(clases){
  return( rbind(
    analisis(clases,crabs.hclusts),
    analisis(clases,lcrabs.hclusts),
    analisis(clases,scrabs.hclusts),
    analisis(clases,pcrabs.hclusts),
    analisis(clases,pcrabs2.hclusts),
    analisis(clases,pcrabs3.hclusts)
  ))
}
analizartodo_hclustc <- function(clases){
  return( rbind(
    analisis(clases,crabs.hclustc),
    analisis(clases,lcrabs.hclustc),
    analisis(clases,scrabs.hclustc),
    analisis(clases,pcrabs.hclustc),
    analisis(clases,pcrabs2.hclustc),
    analisis(clases,pcrabs3.hclustc)
  ))
}
analizartodo_hclusta <- function(clases){
    return( rbind(
      analisis(clases,crabs.hclusta),
      analisis(clases,lcrabs.hclusta),
      analisis(clases,scrabs.hclusta),
      analisis(clases,pcrabs.hclusta),
      analisis(clases,pcrabs2.hclusta),
      analisis(clases,pcrabs3.hclusta)
))
}

k1 <- analizartodo_kmeans(classes[,1])
k2 <- analizartodo_kmeans(classes[,2])
h1s<- analizartodo_hclusts(classes[,1])
h2s<- analizartodo_hclusts(classes[,2])
h1c<- analizartodo_hclustc(classes[,1])
h2c<- analizartodo_hclustc(classes[,2])
h1a<- analizartodo_hclusta(classes[,1])
h2a<- analizartodo_hclusta(classes[,2])


