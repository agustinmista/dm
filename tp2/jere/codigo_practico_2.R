#-------------------------------------------------------------------------------------
# AVISO: este codigo esta adaptado de un paquete mayor. 
# No es optimo y tiene cosas inutiles para nosotros. Es un ejemplo nada mas
#
#general forward greedy selection function
# x,y inputs and targets
# method is an external function that estimates classification error with a given model
# ... parameters for method
#-------------------------------------------------------------------------------------
forward.ranking <- function(x,y,method,verbosity=0,... )
{
  
    max.feat<-dim(x)[2]       # Cantidad de features
    num.feat<-1               # Cantidad de features elegidas al momento
    list.feat<-1:max.feat      

    #initial ranking
  x.train<-matrix(0,dim(x)[1],1)  # Crea una matriz llena de ceros con n filas y 1 columna
    class.error<-double(max.feat)
    for(i in 1:max.feat){
        x.train[,1]<-x[,i]
        class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
    }
    list.feat[1]<-which.min(class.error)                          #Las features elegidas y en orden!
    keep.feat<-sort(class.error,decreasing=FALSE,index=T)$ix[-1]  #Las features aÃºn no seleccionadas!
    x.prev<-x.train[,1]<-x[,list.feat[1]]                         #Guarda la matrix previa

    if(verbosity>1) cat("\nFirst feature: ",list.feat[1],"\n")

    while(num.feat<max.feat){    # Mientras falte elegir features
        class.error<-double(max.feat-num.feat)
        for(i in 1:(max.feat-num.feat)){   # Para cada feature que aun no ha sido elegidas
            x.train<-cbind(x.prev,x[,keep.feat[i]])  # Pega a la matrix previa  
            class.error[i] <- do.call(method, c(list(x.train, y), list(...)) ) # Saca el error
        }
        if(verbosity>2) cat("\nFeatures:\n",keep.feat,"\nErrors:\n",class.error)
        
        best.index<-which.min(class.error)  # Entruenta la feature c/ mÃ???nimo error
        list.feat[num.feat+1]<-keep.feat[best.index]   # La guarda
        if(verbosity>1) cat("\n---------\nStep ",1+num.feat,"\nFeature ",best.index)

        #Actualiaz todo
        keep.feat<-keep.feat[-best.index] 
        if(verbosity>2) cat("\nNew search list: ",keep.feat)
        num.feat<-num.feat+1
        x.prev<-as.matrix(x[,list.feat[1:num.feat]])
    }
    #Terminado el prceso!

    search.names<-colnames(x)[list.feat] # Recupera el nombre de las features elegidas
    imp<-(max.feat:1)/max.feat           
    names(imp)<-search.names

    if(verbosity>1){
        cat("\n---------\nFinal ranking ",num.feat," features.")
        cat("\nFeatures: ",search.names,"\n")
    }

    return( list(ordered.names.list=search.names,ordered.features.list=list.feat,importance=imp) )

}


#---------------------------------------------------------------------------
# EJERCICIO 1A - BACKWARD GREEDY SELECTION
backward.ranking <- function(x,y,method,verbosity=0,... )
{
  if(verbosity>1) cat("\n Starting backward ranking. Initializing variables.")
  numFeat <- dim(x)[2]
  featuresEliminadas <- c()
  featuresMantenidas <- 1:numFeat
  if(verbosity>2) cat("\n non removed features:",featuresMantenidas, "\ removed features:", featuresEliminadas)

  if(verbosity>2) cat("\n Starting while")
  
  while (length(featuresMantenidas) > 1) {
    if (verbosity>1) cat("\n Step ",length(featuresEliminadas)+1)
    if (verbosity>1) cat("\n removed features: ",featuresEliminadas)
    if (verbosity>1) cat("\n non removed features: ",featuresMantenidas)
    error <- rep(1,numFeat)
    for (i in featuresMantenidas) {
      x.train  <- x[-c(i,featuresEliminadas)]
      error[i] <- do.call(method, c(list(as.matrix(x.train), y), list(...)) )
      if (verbosity>2) cat("\n Considering removing feature ",i,"Calculated error:",error[i])
    }
    worstFeature <- which.min(error)
    if (verbosity>1) cat("\n worstFeature: ",worstFeature)
    featuresEliminadas <- c(worstFeature,featuresEliminadas)
    featuresMantenidas <- featuresMantenidas[!featuresMantenidas==worstFeature]
    
  }

  featuresEliminadas <- c(featuresMantenidas,featuresEliminadas)  # Le agrego la que quedo al final
  
  search.names<-colnames(x)[featuresEliminadas] # Recupera el nombre de las features eliminadas
  imp<-(numFeat:1)/numFeat           
  names(imp)<-search.names
  
  if(verbosity>1){
     cat("\n---------\nFinal ranking ",featuresEliminadas)
     cat("\nFeatures: ",search.names,"\n")
  }
  return( list(ordered.features.list=featuresEliminadas,importance=imp) )
  
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# EJERCICIO 1B - FILTER C/TEST NO PARAMETRIZADO
filter.kruskal <- function(x,y){  
  nFeat    <- dim(x)[2]
  features <- 1:(nFeat)
  error    <- rep(-1,nFeat)
  for (i in features){
     error[i] <- kruskal.test(x[,i],y)$statistic
  }
  return(sort(error, index.return=TRUE, decreasing = TRUE)[2])
}

#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# EJERCICIO 1C - RFE
rfe<- function(x,y,method,verbosity=0,... )
{
  if(verbosity>1) cat("\n Starting RFE. Initializing variables.")
  numFeat <- dim(x)[2]
  featuresEliminadas <- c()
  featuresMantenidas <- 1:numFeat
  if(verbosity>2) cat("\n non removed features",featuresMantenidas, "\ removed features", featuresEliminadas)
  
  if(verbosity>2) cat("\n Starting while")
  
  while (length(featuresMantenidas) > 1) {
    if (verbosity>1) cat("\n Step ",length(featuresEliminadas)+1)
    if (verbosity>1) cat("\n removed features: ",featuresEliminadas)
    if (verbosity>1) cat("\n non removed features: ",featuresMantenidas)
    rank <- do.call(method, c(list(x[,featuresMantenidas], y), list(...)) )
    worstFeatureix <- (rank$feats)[1]
    worstFeature <- featuresMantenidas[worstFeatureix]
    if (verbosity>1) cat("\n worstFeature: ",worstFeature)
    featuresEliminadas <- c(worstFeature,featuresEliminadas)
    featuresMantenidas <- featuresMantenidas[!featuresMantenidas==worstFeature]
    
  }
  
  featuresEliminadas <- c(featuresMantenidas,featuresEliminadas)  # Le agrego la que quedo al final
  
  search.names<-colnames(x)[featuresEliminadas] # Recupera el nombre de las features eliminadas
  imp<-(numFeat:1)/numFeat           
  names(imp)<-search.names
  
  if(verbosity>1){
    cat("\n---------\nFinal ranking ",featuresEliminadas)
    cat("\nFeatures: ",search.names,"\n")
  }
  return( list(ordered.features.list=featuresEliminadas,importance=imp) )

}
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
#random forest error estimation (OOB) for greedy search
#---------------------------------------------------------------------------
rf.est <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
    if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
    prop.samples<-table(y)
    if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))
    return( randomForest(x.train,y,mtry=mtry,ntree=tot.trees,sampsize=prop.samples)$err.rate[tot.trees] )
}

#---------------------------------------------------------------------------
#LDA error estimation (LOO) for greedy search
#---------------------------------------------------------------------------
lda.est <- function(x.train,y)
{
    m.lda <- lda(x.train,y,CV=TRUE)
    return(error.rate( y , m.lda$class))
}
error.rate <- function(dataA, dataB) sum( dataA != dataB ) / length(dataB)

#---------------------------------------------------------------------------
#SVM error estimation (internal CV) for greedy search
#---------------------------------------------------------------------------
svm.est <- function(x.train,y,type="C-svc",kernel="vanilladot",C=1,cross = 4)
{
    return ( ksvm(x.train, y, type=type,kernel=kernel,C=C,cross = cross)@cross )
}


#---------------------------------------------------------------------------
#random forest ranking method for rfe.
#---------------------------------------------------------------------------
imp.rf <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
    if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
    prop.samples<-table(y)
    if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))
    
    m.rf<-randomForest(x.train,y,ntree=tot.trees,mtry=mtry,sampsize=prop.samples,importance=TRUE)
    imp.mat<-importance(m.rf)
    imp.col<-dim(imp.mat)[2]-1
    rank.list<-sort(imp.mat[,imp.col],decreasing=FALSE,index=T)
    return(list(feats=rank.list$ix,imp=rank.list$x))
}


#---------------------------------------------------------------------------
#linear svm ranking method for rfe. Using kernlab. Multiclass
#---------------------------------------------------------------------------
imp.linsvm <- function(x.train,y,C=100)
{
    num.feat<-dim(x.train)[2]
    tot.problems<-nlevels(y)*(nlevels(y)-1)/2

    m.svm <- ksvm(as.matrix(x.train), y, type="C-svc",kernel="vanilladot",C=C)

    w<-rep(0.0,num.feat)
    for(i in 1:tot.problems) for(feat in 1:num.feat)
        w[feat]<-w[feat]+abs(m.svm@coef[[i]] %*% m.svm@xmatrix[[i]][,feat])
    rank.list<-sort(w,decreasing=FALSE,index=T)
    return(list(feats=rank.list$ix,imp=rank.list$x))
}



library(randomForest)
library(kernlab)
library(MASS)

#demo: aplicar el wrapper a los datos de iris
data(iris)

#FORW.rf   <- forward.ranking(iris[,-5],iris[,5],method="rf.est" ,tot.trees=100,equalize.classes=F)
#FORW.lda  <- forward.ranking(iris[,-5],iris[,5],method="lda.est")
#BACKW.rf  <- backward.ranking(iris[,-5],iris[,5],method="rf.est" ,tot.trees=100,equalize.classes=F)
#BACKW.lda <- backward.ranking(iris[,-5],iris[,5],method="lda.est")
#FILT      <- filter.kruskal(iris[,-5],iris[,5])
#RFE.rf    <- rfe(iris[,-5],iris[,5],method="imp.rf",equalize.classes=F,tot.trees=100)
#RFE.svm   <- rfe(iris[,-5],iris[,5],method="imp.linsvm")

#hacer una funcion que cree datos, 2 clases (-1 y 1,n puntos de cada una), d dimensiones, de ruido uniforme [-1,1], con la clase al azar

crea.ruido.unif<-function(n=100,d=2){
   x<-runif(2*n*d,min=-1)  #genero los datos entre -1 y 1, el primer param es la cantidad de obs
   dim(x)<-c(2*n,d)
   return(cbind(as.data.frame(x),y=factor(rep(c(-1,1),each=n))))   #le agrego la clase
}

#datosA
d<-10
n<-1000
datos<-crea.ruido.unif(n=n,d=d)

#tomar 50% de los datos al azar, y hacer que la clase sea el signo de la 8 variable
shuffle<-sample(1:dim(datos)[1])
sub<-shuffle[1:dim(datos)[1]*0.5]
datos[sub,d+1]<-sign(datos[sub,8])
#tomar 20% de los datos al azar (fuera de los anteriores), y hacer que la clase sea el signo de la 6 variable
sub<-shuffle[(dim(datos)[1]*0.5):(dim(datos)[1]*0.7)]
datos[sub,d+1]<-sign(datos[sub,6])
#tomar 10% de los datos al azar, y hacer que la clase sea el signo de la 4 variable
sub<-shuffle[(dim(datos)[1]*0.7):(dim(datos)[1]*0.8)]
datos[sub,d+1]<-sign(datos[sub,4])
#tomar 5% de los datos al azar, y hacer que la clase sea el signo de la 2 variable
sub<-shuffle[(dim(datos)[1]*0.8):(dim(datos)[1]*0.85)]
datos[sub,d+1]<-sign(datos[sub,2])
datos[,d+1]<-factor(datos[,d+1])

datosA<-datos

#datosB
#generar n=100,d=8
d<-8
n<-1000
datos<-crea.ruido.unif(n=n,d=d)
#hacer que la clase sea el xor de las 2 primeras variables (es usando el signo)
datos[,d+1]<-sign(datos[,1]*datos[,2])
#hacer que las variables 3 y 4 tengan un 50% de correlacion con la clase
shuffle<-sample(1:dim(datos)[1])
sub<-shuffle[1:dim(datos)[1]*0.5]
datos[sub,3]<-abs(datos[sub,3])*datos[sub,d+1]
shuffle<-sample(1:dim(datos)[1])
sub<-shuffle[1:dim(datos)[1]*0.5]
datos[sub,4]<-abs(datos[sub,4])*datos[sub,d+1]
datos[,d+1]<-factor(datos[,d+1])

datosB<-datos


# EJERCICIO 2

#Aplica todos los métodos y printea los rankings
aplicarMetodos <- function(dataset=iris){
  nClases = dim(dataset)[2]
  
  cat("Forward rf ", forward.ranking(dataset[,-nClases],dataset[,nClases],method="rf.est" ,tot.trees=100,equalize.classes=F)$ordered.features.list,"\n")
  cat("Forward lda ", forward.ranking(dataset[,-nClases],dataset[,nClases],method="lda.est")$ordered.features.list,"\n")
  cat("Forward svm ",  forward.ranking(dataset[,-nClases],dataset[,nClases],method="svm.est")$ordered.features.list,"\n")
  cat("Backward rf ", backward.ranking(dataset[,-nClases],dataset[,nClases],method="rf.est" ,tot.trees=100,equalize.classes=F)$ordered.features.list,"\n")
  cat("Backward lda ", backward.ranking(dataset[,-nClases],dataset[,nClases],method="lda.est")$ordered.features.list,"\n")
  cat("Backward svm", backward.ranking(dataset[,-nClases],dataset[,nClases],method="svm.est")$ordered.features.list,"\n")
  cat("Filtro Kruskal ", filter.kruskal(dataset[,-nClases],dataset[,nClases])$ix,"\n")
  cat("RFE rf ", rfe(dataset[,-nClases],dataset[,nClases],method="imp.rf",equalize.classes=F,tot.trees=100)$ordered.features.list,"\n")
  cat("RFE svm ", rfe(dataset[,-nClases],dataset[,nClases],method="imp.linsvm")$ordered.features.list,"\n")
}

# aplicarMetodos(datosA)
# aplicarMetodos(datosB)

# EJERCICIO 3: en ej3.R

# OPCIONAL

#Recupero el dataset
student <- read.csv(file="C:/Users/Jeremías/Dropbox/FACU_2017/DM/DM_TP2_Jeremias/student/student-mat.csv", header=TRUE, sep=";")

#Tiro las columnas que no voy a usar
target  <- student$G3
student <- subset(student, select=-c(Mjob,Fjob,reason,G1,G2,G3))


#Convertimos variables categóricas en variables 0-1

student$school                        <- as.integer(student$school)        #  1 = GP
student$school[student$school==2]     <- 0                                 #  0 = MS
 
student$address                       <- as.integer(student$address)       #  1 = F
student$address[student$address==2]   <- 0                                 #  0 = M

student$sex                       <- as.integer(student$sex)               #  1 = F
student$sex[student$sex==2]       <- 0                                     #  0 = M

student$famsize                       <- as.integer(student$famsize)       #  1 = GT3
student$famsize[student$famsize==2]   <- 0                                 #  0 = LE3

student$Pstatus                       <- as.integer(student$Pstatus)       #  1 = GT3
student$Pstatus[student$Pstatus==2]   <- 0                                 #  0 = LE3


student$schoolsup                       <- as.integer(student$schoolsup)   #  1 = GT3
student$schoolsup[student$schoolsup==2]   <- 0                             #  0 = LE3


student$famsup                       <- as.integer(student$famsup)         #  1 = GT3
student$famsup[student$famsup==2]   <- 0                                   #  0 = LE3


student$paid                       <- as.integer(student$paid)             #  1 = GT3
student$paid[student$paid==2]   <- 0                                       #  0 = LE3

student$activities                       <- as.integer(student$activities)       #  1 = GT3
student$activities[student$activities==2]   <- 0                                 #  0 = LE3

student$nursery                       <- as.integer(student$nursery)       #  1 = GT3
student$nursery[student$nursery==2]   <- 0                                 #  0 = LE3

student$higher                       <- as.integer(student$higher)           #  1 = GT3
student$higher[student$higher==2]   <- 0                                     #  0 = LE3

student$internet                       <- as.integer(student$internet)       #  1 = GT3
student$internet[student$internet==2]   <- 0                                 #  0 = LE3

student$romantic                       <- as.integer(student$romantic)       #  1 = GT3
student$romantic[student$romantic==2]   <- 0                                 #  0 = LE3

# Convertimos la variable categorica "guardian" que asume valores father mother and other en tres variables 0-1

guardian_father <- as.integer(student$guardian)
guardian_mother <- as.integer(student$guardian)
guardian_other  <- as.integer(student$guardian)

guardian_father[guardian_father!=1] <- 0
guardian_mother[guardian_mother!=2] <- 0
guardian_mother[guardian_mother==2] <- 1
guardian_other[guardian_other!=3]   <- 0
guardian_other[guardian_other==3]   <- 1

student <- subset(student, select=-c(guardian))
student <- cbind(student,guardian_father,guardian_mother,guardian_other)

# Llevo al 0-1 las variables que estan fuera de ese rango

student$health <- (student$health - min(student$health))/(max(student$health)-min(student$health))
student$studytime <- (student$studytime - min(student$studytime))/(max(student$studytime)-min(student$studytime))
student$failures <- (student$failures - min(student$failures))/(max(student$failures)-min(student$failures))
student$famrel <- (student$famrel - min(student$famrel))/(max(student$famrel)-min(student$famrel))
student$freetime <- (student$freetime - min(student$freetime))/(max(student$freetime)-min(student$freetime))
student$goout <- (student$goout - min(student$goout))/(max(student$goout)-min(student$goout))
student$Dalc <- (student$Dalc - min(student$Dalc))/(max(student$Dalc)-min(student$Dalc))
student$Walc <- (student$Walc - min(student$Walc))/(max(student$Walc)-min(student$Walc))
student$absences <- (student$absences - min(student$absences))/(max(student$absences)-min(student$absences))
student$Medu <- (student$Medu - min(student$Medu))/(max(student$Medu)-min(student$Medu))
student$Fedu <- (student$Fedu - min(student$Fedu))/(max(student$Fedu)-min(student$Fedu))
student$traveltime <- (student$traveltime - min(student$traveltime))/(max(student$traveltime)-min(student$traveltime))
student$age <- (student$age - min(student$age))/(max(student$age)-min(student$age))

# Armamos la variable que queremos clasificar

isSmart <- function(n){
  if (n>15) return(1)
  else return(0)
}

smart <- as.factor(sapply(target,isSmart))
student<-cbind(student,smart)

# Calculamos los rankings
aplicarMetodos(student)
