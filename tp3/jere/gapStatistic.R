load("lampone.Rdata")
data(iris)

# method :: Dataset, K -> wk
gapStatistic <- function(dataset,K,B,method){
  
   # STEP 1 - correr el método en para todo k, calculando la suma de los pesos internos wk.
   wk  <- 0 
   dataset <- prcomp(dataset,retx=TRUE)$x
   for (k in 1:K){
     wk[k] <-method(dataset,k)
   }

   #STEP 2 - correr el método B veces para cada k, en conjuntos uniformes sampleados sobre la PCA 
   wkb <- matrix(nrow=K,ncol=B)
   for (k in 1:K){
     for(b in 1:B){
       wkb[k,b] <- method(generarUniforme(dataset),k)
     }
   }
   
   #calcular el gap
   gap <- 0
   
   l  <- 0
   sd <- rep(0,K)
   s  <- 0
   
   
   o <- log(wk)
   l <- rowSums(log(wkb))/B #uniform average
   
   gap <- l - o

   #calculo cosas estadisticas (?????)
   for (k in 1:K){
     sd[k]  <- ((1/B)*sum((log(wkb[k,])-l[k])**2))**(1/2)
     s[k]   <- sd[k]*(1+1/B)**(1/2)
   }
   
   #elegir el k
   for (k in 1:(K-1)){
     if(gap[k]>=gap[k+1]-s[k+1])
       return(k)
   }
  return(0)
}

#funcion auxiliar de gapStatistic
generarUniforme <- function(ds){
  nvars <- dim(ds)[2]
  npts  <- dim(ds)[1]

  max <- 0
  min <- 0

  for (i in 1:nvars){
     max[i] <- max(ds[,i])
     min[i] <- min(ds[,i])
  }
  
  cols <- matrix(0,nrow=npts,ncol=nvars)
  
  for (i in 1:nvars){
    cols[,i] <- runif(npts,min[i],max[i])
  }
  return(cols) 
}

#Argumentos para gapStatistic
methodKmeans <- function(dataset,k){
  return(kmeans(dataset,k,nstart=100)$tot.withinss)
}
methodHclustS <- function(dataset,k){
  nFeat <- dim(dataset)[2]
  npoints <- dim(dataset)[1]
  
  clas <- cutree(hclust(dist(dataset),"single"),k=k)
  whole <- cbind(dataset,clas)
  
  suma <- 0
  for(i in 1:k){
    if(!is.null(dim(whole[whole[,nFeat+1] == i,]))) {
      suma    <- suma +  sum(dist(whole[whole[,nFeat+1] == i,][,1:nFeat]))
    }
  }
  #Falta implementar alguna forma de calcular los withinss
  return(suma)
}
methodHclustA <- function(dataset,k){
  nFeat <- dim(dataset)[2]
  npoints <- dim(dataset)[1]
  
  clas <- cutree(hclust(dist(dataset),"average"),k=k)
  whole <- cbind(dataset,clas)
  
  suma <- 0
  for(i in 1:k){
    if(!is.null(dim(whole[whole[,nFeat+1] == i,]))) {
      suma    <- suma +  sum(dist(whole[whole[,nFeat+1] == i,][,1:nFeat]))
    }
  }
  #Falta implementar alguna forma de calcular los withinss
  return(suma)
}
methodHclustC <- function(dataset,k){
  nFeat <- dim(dataset)[2]
  npoints <- dim(dataset)[1]
  
  clas <- cutree(hclust(dist(dataset),"complete"),k=k)
  whole <- cbind(dataset,clas)
  
  suma <- 0
  for(i in 1:k){
    if(!is.null(dim(whole[whole[,nFeat+1] == i,]))) {
      suma    <- suma +  sum(dist(whole[whole[,nFeat+1] == i,][,1:nFeat]))
    }
  }
  #Falta implementar alguna forma de calcular los withinss
  return(suma)
}

#EJ3 - Aplicación

# Lampone
  classes <- lampone[,c(1,143)]
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


# Cuatro Gaussianas
tot.puntos<-100
gap=2
x<-rnorm(tot.puntos,mean=-gap)
y<-rnorm(tot.puntos,mean=-gap)
gausianas<-cbind(x,y,rep(1,length(x)))
x<-rnorm(tot.puntos,mean=2*gap)
y<-rnorm(tot.puntos,mean=0)
gausianas<-rbind(gausianas,cbind(x,y,rep(2,length(x))))
x<-rnorm(tot.puntos,mean=0.7*gap,sd=0.5)
y<-rnorm(tot.puntos,mean=2.5*gap,sd=0.5)
gausianas<-rbind(gausianas,cbind(x,y,rep(3,length(x))))
x<-rnorm(tot.puntos,mean=-gap,sd=0.5)
y<-rnorm(tot.puntos,mean=gap,sd=0.5)
gausianas<-rbind(gausianas,cbind(x,y,rep(4,length(x))))
plot(gausianas[,1:2],col=gausianas[,3])

ej3a <- function(N,method){
  res <- matrix(nrow=5,ncol=10)
  for(i in 1:N){
    res[1,i] <- gapStatistic(gausianas[,1:2],5,5,method)
    res[2,i] <- gapStatistic(scale(log(iris[,1:4])),10,10,method)
    res[3,i] <- gapStatistic(scale(log(lamp+1)),10,10,method)
    res[4,i] <- gapStatistic(iris[,1:4],10,10,method)
    res[5,i] <- gapStatistic(lamp,10,10,method)
  }
  return(res)
}
