#Estabilidad
load("lampone.Rdata")
stability <- function(dataset,K,B,method,proporcion=0.9){
  
  combs <- combn(B,2)
  ncombs <- dim(combs)[2]
  scores <- matrix(nrow=K,ncol=ncombs) # scores de todas las combinaciones para cada k
  
  n<-dim(dataset)[1]  # Numero de puntos
  
  ind <- matrix(nrow=B,ncol=proporcion*n) # Los indices de los B datasets generados
  
  for(b in 1:B){
    ind[b,] <- sample(n,proporcion*n) 
  }
  for(k in 1:K){
    for(comb in 1:ncombs){ # para cada k calculo el score de cada combinación y lo guardo en score.
      
      nc1 <- combs[,comb][1]
      nc2 <- combs[,comb][2] 
      ind1 <- ind[nc1,]
      ind2 <- ind[nc2,]   

      cc1<-method(dataset[ind1,],k)
      cc2<-method(dataset[ind2,],k)
      
      #pongo los clusters de nuevo en longitud n - quedan 0 los puntos fuera del sample
      v1<-v2<-rep(0,n)
      v1[ind1]<-cc1
      v2[ind2]<-cc2
      #creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster 
      #y 0 si alguno no esta, para cada clustering
      
      a<-sqrt(v1%*%t(v1))
      m1<-a / -a + 2*(a==round(a))
      m1[is.nan(m1)]<-0
      a<-sqrt(v2%*%t(v2))
      m2<-a / -a + 2*(a==round(a))
      m2[is.nan(m2)]<-0
      #calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
      validos<-sum(v1*v2>0)
      scores[k,comb]<-sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
      
    }
  }
  return(scores)
  
}

#argumentos para la función stability
mKmeans <- function(dataset,k){
  return(kmeans(dataset,k,nstart=10)$cluster)
}
mHclustS <- function(dataset,k){
  return(cutree(hclust(dist(dataset),"single"),k=k))
}
mHclustA <- function(dataset,k){
  return(cutree(hclust(dist(dataset),"average"),k=k))
}
mHclustC <- function(dataset,k){
  return(cutree(hclust(dist(dataset),"complete"),k=k))
}


#plotea la matriz de scores para interpretarla
plot_curvasHIST <- function(scores){
  K      <- dim(scores)[1]
  ncombs <- dim(scores)[2]
  scoresY <- scores
  for(i in 1:K){
    scores[i,]  <- sort(scores[i,])
    scoresY[i,] <- cumsum(1:ncombs)
    scoresY[i,] <- scoresY[i,]/max(scoresY[i,])  
  }
  plot(cbind(scores[1,],scoresY[1,]),col=1,type="l",xlim=c(0.5,1), ylim=c(0,1)
      ,xlab="scores",ylab="cumulative")
  for(i in 2:K){ lines(cbind(scores[i,],scoresY[i,]),col=i)}
  legend(  x="topleft",
           legend=c("1","2","3","4","5","6","7","8","9","10")
           , col=1:10
           , lty=rep("solid",10)
           , lwd=3
           , pch=16 
  )
}


#ej3 - aplicacion


#Gaussianas
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




#Iris
data(iris)


#Lampone
classes <- lampone[,c(1,143)]
nums <- sapply(lampone, is.numeric)
lamp <- lampone[ , nums]
i <- (colSums(lamp, na.rm=T) != 0)
lamp <- lamp[, i] 


#DESCOMENTAR!!! Pero tarda un rato en calcular.
#l <- stability(lamp,5,15,mHclustS,0.7)
#i <- stability(iris[,1:4],10,15,mHclustS,0.9)
#g <- stability(gausianas[,1:2],10,15,mHclustS,0.9)


