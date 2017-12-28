source("codigo_practico_2.R")

# EJERCICIO 3 


# Esta funcion genera un dataset del problema Diagonal
genera_diagonal <- function(C,d,n) {
  
  ds <- C*sqrt(d)     # Desviacion estandar
  n0 <- ceiling(n/2)  # Numero de puntos de clase 0
  n1 <- n/2           # Numero de puntos de clase 1
  
  puntos0 <-  matrix( replicate(n0, rnorm(d,-1,ds)) , ncol=d , byrow = T)  # puntos de clase 0  
  puntos1 <-  matrix( replicate(n1, rnorm(d, 1,ds)) , ncol=d , byrow = T)  # puntos de clase 1
  
  filas0  <-  cbind (puntos0, rep(0,n0))   # filas finales del dataset
  filas1  <-  cbind (puntos1, rep(1,n1))   # filas finales del dataset
  
  dataset <- rbind(filas0,filas1)   
  colnames(dataset)[d+1] <- "c"   # Renombro esta columna para usar rpart
  return(data.frame(dataset))
}


genera_dataset_ej3 <- function(){
  diag <- genera_diagonal(2/sqrt(10),10,100)  # Desv. estandar =   C . sqrt(d) =  2/sqrt(10)*sqrt(10) = 2
  x<-runif(100*90,min=-1)	
  dim(x)<-c(100,90)
  datos <- cbind (diag[,1:10],x,diag[,"c"])
  names(datos) <- 1:101
  return(datos)
}



f.rf <- f.lda <- f.svm <- b.rf <- b.lda <- b.svm <- f.k <- rfe.rf <- rfe.svm <- rep(0,10) 

for (i in 1:10) {
  cat("\n ITERACIÓN ",i)
  datos <- genera_dataset_ej3()
  x<- datos[,1:100]
  y<- factor(datos[,101])
  f.rf[i]  <- sum(forward.ranking(x,y,method="rf.est" ,tot.trees=100,equalize.classes=F)$ordered.features.list[1:10] < 11)
  cat("!")
  f.lda[i] <- sum(forward.ranking(x,y,method="lda.est")$ordered.features.list[1:10] < 11)
  cat("!")
  f.svm[i] <- sum(forward.ranking(x,y,method="svm.est")$ordered.features.list[1:10] < 11)
  cat("!")
  b.rf[i]  <- sum(backward.ranking(x,y,method="rf.est" ,tot.trees=100,equalize.classes=F)$ordered.features.list[1:10] < 11)
  cat("!")
  b.lda[i] <- sum(backward.ranking(x,y,method="lda.est")$ordered.features.list[1:10] < 11)
  cat("!")
  b.svm[i] <- sum (backward.ranking(x,y,method="svm.est")$ordered.features.list[1:10] < 11)
  cat("!")
  f.k[i] <- sum(filter.kruskal(x,y)$ix[1:10] < 11)
  cat("!")
  rfe.rf[i] <- sum(rfe(x,y,method="imp.rf",equalize.classes=F,tot.trees=100)$ordered.features.list[1:10] < 11)
  cat("!")
  rfe.svm[i] <- sum(rfe(x,y,method="imp.linsvm")$ordered.features.list[1:10] < 11)
  cat("!")
}

cat("\n Porcentaje de aciertos:")
cat("\n FORWARD RF :",mean(f.rf)/10)
cat("\n FORWARD LDA :",mean(f.lda)/10)
cat("\n FORWARD SVM :",mean(f.svm)/10)
cat("\n BACKWARD RF :",mean(b.rf)/10)
cat("\n BACKWARD LDA :",mean(b.lda)/10)
cat("\n BACKWARD SVM :",mean(b.svm)/10)
cat("\n FILTER KRK :",mean(f.k)/10)
cat("\n RFE RF :",mean(rfe.rf)/10)
cat("\n RFE SVM :",mean(rfe.svm)/10)

