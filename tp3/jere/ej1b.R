load("lampone.Rdata")
library(MASS)

classes <- lampone[,c(1,143)]

#Raw dataset, eliminando las  columnas no-numéricas, todas cero o con algún otro problema
nums <- sapply(lampone, is.numeric)
lamp <- lampone[ , nums]
i <- (colSums(lamp, na.rm=T) != 0)
lamp <- lamp[, i] 

#Log
llamp <- log(lamp+1)
#Log + scale
slamp <- data.frame(scale(llamp,center=TRUE,scale=TRUE))
#Log + scale + pca
plamp <- data.frame(prcomp(llamp,retx=TRUE,scale=TRUE)$x)
#Log + pca
plamp2 <- data.frame(prcomp(llamp,retx=TRUE,scale=FALSE)$x)
#Log + pca + scale
plamp3 <- data.frame(scale(prcomp(llamp,retx=TRUE,scale=FALSE)$x))


# APLICAMOS AMBOS MÉTODOS A TODOS LOS DATASETS

lamp.kmeans   <- kmeans(lamp[4:8],2,50)$cluster
llamp.kmeans  <- kmeans(llamp,2,50)$cluster
slamp.kmeans  <- kmeans(slamp,2,50)$cluster
plamp.kmeans  <- kmeans(plamp,2,50)$cluster
plamp2.kmeans <- kmeans(plamp2,2,50)$cluster
plamp3.kmeans <- kmeans(plamp3,2,50)$cluster

lamp.hclusts   <- cutree(hclust(dist(lamp[4:8]),"single"),k=2)
llamp.hclusts  <- cutree(hclust(dist(llamp),"single"),k=2)
slamp.hclusts  <- cutree(hclust(dist(slamp),"single"),k=2)
plamp.hclusts  <- cutree(hclust(dist(plamp),"single"),k=2)
plamp2.hclusts <- cutree(hclust(dist(plamp2),"single"),k=2)
plamp3.hclusts <- cutree(hclust(dist(plamp3),"single"),k=2)

lamp.hclustc   <- cutree(hclust(dist(lamp[4:8]),"complete"),k=2)
llamp.hclustc  <- cutree(hclust(dist(llamp),"complete"),k=2)
slamp.hclustc  <- cutree(hclust(dist(slamp),"complete"),k=2)
plamp.hclustc  <- cutree(hclust(dist(plamp),"complete"),k=2)
plamp2.hclustc <- cutree(hclust(dist(plamp2),"complete"),k=2)
plamp3.hclustc <- cutree(hclust(dist(plamp3),"complete"),k=2)

lamp.hclusta   <- cutree(hclust(dist(lamp[4:8]),"average"),k=2)
llamp.hclusta  <- cutree(hclust(dist(llamp),"average"),k=2)
slamp.hclusta  <- cutree(hclust(dist(slamp),"average"),k=2)
plamp.hclusta  <- cutree(hclust(dist(plamp),"average"),k=2)
plamp2.hclusta <- cutree(hclust(dist(plamp2),"average"),k=2)
plamp3.hclusta <- cutree(hclust(dist(plamp3),"average"),k=2)


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
    analisis(clases,lamp.kmeans),
    analisis(clases,llamp.kmeans),
    analisis(clases,slamp.kmeans),
    analisis(clases,plamp.kmeans),
    analisis(clases,plamp2.kmeans),
    analisis(clases,plamp3.kmeans)
  ) )
}

analizartodo_hclusts <- function(clases){
  return( rbind(
    analisis(clases,lamp.hclusts),
    analisis(clases,llamp.hclusts),
    analisis(clases,slamp.hclusts),
    analisis(clases,plamp.hclusts),
    analisis(clases,plamp2.hclusts),
    analisis(clases,plamp3.hclusts)
  ))
}

analizartodo_hclustc <- function(clases){
  return( rbind(
    analisis(clases,lamp.hclustc),
    analisis(clases,llamp.hclustc),
    analisis(clases,slamp.hclustc),
    analisis(clases,plamp.hclustc),
    analisis(clases,plamp2.hclustc),
    analisis(clases,plamp3.hclustc)
  ))
}

analizartodo_hclusta <- function(clases){
  return( rbind(
    analisis(clases,lamp.hclusta),
    analisis(clases,llamp.hclusta),
    analisis(clases,slamp.hclusta),
    analisis(clases,plamp.hclusta),
    analisis(clases,plamp2.hclusta),
    analisis(clases,plamp3.hclusta)
  ))
}

k1 <- analizartodo_kmeans(lampone[,1])
k2 <- analizartodo_kmeans(lampone[,143])
h1s<- analizartodo_hclusts(lampone[,1])
h2s<- analizartodo_hclusts(lampone[,143])
h1c<- analizartodo_hclustc(lampone[,1])
h2c<- analizartodo_hclustc(lampone[,143])
h1a<- analizartodo_hclusta(lampone[,1])
h2a<- analizartodo_hclusta(lampone[,143])

