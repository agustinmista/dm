library(randomForest)
library(kernlab)
library(MASS)

#-------------------------------------------------------------------------------
# AVISO: este codigo esta adaptado de un paquete mayor. No es optimo y tiene
# cosas inutiles para nosotros. Es un ejemplo nada mas
#
# general forward greedy selection function x,y inputs and targets method is an
# external function that estimates classification error with a given model ...
# parameters for method
# ------------------------------------------------------------------------------
forward.ranking <- function(x,y,method,verbosity=0, ...) {

	max.feat <- dim(x)[2]
	num.feat <- 1
	list.feat <- 1:max.feat

	#initial ranking
  x.train <- matrix(0, dim(x)[1], 1)
	class.error <- double(max.feat)

  for (i in 1:max.feat) {
		x.train[,1] <- x[,i]
		class.error[i] <- do.call(method, c(list(x.train, y), list(...)))
	}

  list.feat[1] <- which.min(class.error)
	keep.feat <- sort(class.error, decreasing=FALSE, index=TRUE)$ix[-1]
	x.prev <- x.train[,1] <- x[,list.feat[1]]

	if (verbosity>1) cat("\nFirst feature: ", list.feat[1], "\n")

	while (num.feat<max.feat) {
		class.error <- double(max.feat-num.feat)

    for (i in 1:(max.feat-num.feat)) {
			x.train <- cbind(x.prev, x[,keep.feat[i]])
			class.error[i] <- do.call(method, c(list(x.train, y), list(...)))
		}

    if (verbosity > 2)
      cat("\nFeatures:\n", keep.feat, "\nErrors:\n", class.error)

		best.index <- which.min(class.error)
		list.feat[num.feat+1] <- keep.feat[best.index]
		if(verbosity > 1)
      cat("\n---------\nStep ", 1+num.feat, "\nFeature ", best.index)

		keep.feat <- keep.feat[-best.index]
		if(verbosity > 2) cat("\nNew search list: ", keep.feat)
		num.feat <- num.feat + 1
		x.prev <- x[,list.feat[1:num.feat]]
	}


	search.names <- colnames(x)[list.feat]
	imp <- (max.feat:1) / max.feat
	names(imp) <- search.names

	if (verbosity > 1) {
		cat("\n---------\nFinal ranking ", num.feat, " features.")
		cat("\nFeatures: ", search.names, "\n")
	}

 	return(list(ordered.names.list=search.names,
              ordered.features.list=list.feat,
              importance=imp))
}


#-------------------------------------------------------------------------------
# random forest error estimation (OOB) for greedy search
#-------------------------------------------------------------------------------
rf.est <- function(x.train, y, equalize.classes=TRUE, tot.trees=500, mtry=0) {
	if (mtry < 1) mtry <- floor(sqrt(dim(x.train)[2]))
	prop.samples <- table(y)
	if (equalize.classes)
    prop.samples <- rep(min(prop.samples), length(prop.samples))
	return(randomForest(x.train, y, mtry=mtry,ntree=tot.trees,
                      sampsize=prop.samples)$err.rate[tot.trees])
}


#-------------------------------------------------------------------------------
# LDA error estimation (LOO) for greedy search
#-------------------------------------------------------------------------------
lda.est <- function(x.train,y) {
	m.lda <- lda(x.train, y, CV=TRUE)
	return(error.rate(y, m.lda$class))
}

error.rate <- function(dataA, dataB) sum(dataA != dataB) / length(dataB)


#-------------------------------------------------------------------------------
# SVM error estimation (internal CV) for greedy search
#-------------------------------------------------------------------------------
svm.est <- function(x.train, y, type="C-svc", kernel="vanilladot", C=1, cross=4) {
  ranking <- ksvm(as.matrix(x.train), y, type=type, kernel=kernel, C=C, cross=cross)
  return(ranking@cross)
}


#-------------------------------------------------------------------------------
# random forest ranking method for rfe.
#-------------------------------------------------------------------------------
imp.rf <- function(x.train, y, equalize.classes=TRUE, tot.trees=500, mtry=0) {
	if (mtry < 1) mtry <- floor(sqrt(dim(x.train)[2]))
	prop.samples <- table(y)
	if (equalize.classes)
    prop.samples <- rep(min(prop.samples), length(prop.samples))

	m.rf <- randomForest(x.train, y, ntree=tot.trees, mtry=mtry,
                       sampsize=prop.samples, importance=TRUE)
	imp.mat <- importance(m.rf)
	imp.col <- dim(imp.mat)[2]-1
	rank.list <- sort(imp.mat[,imp.col], decreasing=FALSE, index=TRUE)
	return(list(feats=rank.list$ix, imp=rank.list$x))
}


#-------------------------------------------------------------------------------
# linear svm ranking method for rfe. Using kernlab. Multiclass
#-------------------------------------------------------------------------------
imp.linsvm <- function(x.train, y, C=100) {
	num.feat <- dim(x.train)[2]
	tot.problems <- nlevels(y) * (nlevels(y) - 1) / 2

	m.svm <- ksvm(as.matrix(x.train), y, type="C-svc", kernel="vanilladot", C=C)

	w <- rep(0.0, num.feat)
	for (i in 1:tot.problems)
    for (feat in 1:num.feat)
      w[feat] <- w[feat] + abs(m.svm@coef[[i]] %*% m.svm@xmatrix[[i]][,feat])

  rank.list <- sort(w, decreasing=FALSE, index=TRUE)
	return(list(feats=rank.list$ix, imp=rank.list$x))
}


#-------------------------------------------------------------------------------
# backward greedy selection
#-------------------------------------------------------------------------------
backward.ranking <- function(x, y, method, ...) {

  features <- dim(x)[2]
  removed <- c()
  kept <- 1:features

  for (i in 1:(length(kept)-1)) {
    cat("----------------------------------------\n")
    cat("Step :=", i, "\n")

    error <- rep(NULL, features)
    for (i in kept) {
      x.train <- x[-c(i, removed)]
      error[i] <- do.call(method, c(list(as.matrix(x.train), y), list(...)))
    }

    worst <- which.min(error)
    cat("Worst feature :=", worst, "\n")

    kept <- kept[!(kept == worst)]
    cat("Kept features :=", kept, "\n")

    removed <- c(worst, removed)
    cat("Removed features :=", removed, "\n")

  }

  ranking <- c(kept, removed)

  search.names <- colnames(x)[ranking]
  importance <- (features:1) / features
  names(importance) <- search.names

  cat("========================================\n")
  cat("Final ranking :=", ranking, "\n")
  cat("Features :=", search.names, "\n")
  cat("========================================\n")

  return(list(ordered.features.list=ranking, importance=importance))
}


#-------------------------------------------------------------------------------
# non-parametric test filter (Kruskal-Wallis)
#-------------------------------------------------------------------------------
kruskal.filter <- function(x, y) {
  features <- dim(x)[2]
  errors <- rep(NULL, features)

  for (i in 1:features) errors[i] <- kruskal.test(x[,i],y)$statistic

  sorted <- sort(errors, decreasing=TRUE, index.return=TRUE)
  return(list(ordered.features.list=sorted[2]$ix))
}


#-------------------------------------------------------------------------------
# recusive feature elimination
#-------------------------------------------------------------------------------
rfe <- function(x, y, method, ...) {

  features <- dim(x)[2]
  removed <- c()
  kept <- 1:features

  for (i in 1:(length(kept)-1)) {
    cat("----------------------------------------\n")
    cat("Step :=", i, "\n")

    rank <- do.call(method, c(list(x[,kept], y), list(...)))
    worstIndex <- (rank$feats)[1]

    worst <- kept[worstIndex]
    cat("Worst feature :=", worst, "\n")

    kept <- kept[!(kept == worst)]
    cat("Kept features :=", kept, "\n")

    removed <- c(worst, removed)
    cat("Removed features :=", removed, "\n")
  }

  ranking <- c(kept, removed)

  search.names <- colnames(x)[ranking]
  importance <- (features:1) / features
  names(importance) <- search.names

  cat("========================================\n")
  cat("Final ranking :=", ranking, "\n")
  cat("Features :=", search.names, "\n")
  cat("========================================\n")

  return(list(ordered.features.list=ranking, importance=importance))
}


#-------------------------------------------------------------------------------
# DATASETS
#-------------------------------------------------------------------------------

crea.ruido.unif <- function(n=100, d=2) {
  # genero los datos
  x <- runif(2 * n * d, min=-1)
  dim(x) <- c(2 * n, d)

  # le agrego la clase
  return(cbind(as.data.frame(x), y=factor(rep(c(-1, 1), each=n))))
}


#-------------------------------------------------------------------------------
# datosA

d <- 10
n <- 1000
datos <- crea.ruido.unif(n=n, d=d)

# tomar 50% de los datos al azar, y hacer que la clase sea el signo de la 8
# variable
shuffle <- sample(1:dim(datos)[1])
sub <- shuffle[1:dim(datos)[1] * 0.5]
datos[sub, d+1] <- sign(datos[sub, 8])

# tomar 20% de los datos al azar (fuera de los anteriores), y hacer que la clase
# sea el signo de la 6 variable
sub <- shuffle[(dim(datos)[1] * 0.5):(dim(datos)[1] * 0.7)]
datos[sub, d+1] <- sign(datos[sub, 6])

# tomar 10% de los datos al azar, y hacer que la clase sea el signo de la 4
# variable
sub <- shuffle[(dim(datos)[1] * 0.7):(dim(datos)[1] * 0.8)]
datos[sub, d+1] <- sign(datos[sub, 4])

# tomar 5% de los datos al azar, y hacer que la clase sea el signo de la 2
# variable
sub <- shuffle[(dim(datos)[1] * 0.8):(dim(datos)[1] * 0.85)]
datos[sub, d+1] <- sign(datos[sub, 2])
datos[,d+1] <- factor(datos[,d+1])

datosA <- datos

#-------------------------------------------------------------------------------
# datosB

# generar n=100,d=8
d <- 8
n <- 1000
datos <- crea.ruido.unif(n=n, d=d)

#hacer que la clase sea el xor de las 2 primeras variables (es usando el signo)
datos[,d+1] <- sign(datos[,1] * datos[,2])

#hacer que las variables 3 y 4 tengan un 50% de correlacion con la clase
shuffle <- sample(1:dim(datos)[1])
sub <- shuffle[1:dim(datos)[1] * 0.5]
datos[sub, 3] <- abs(datos[sub, 3]) * datos[sub, d+1]
shuffle <- sample(1:dim(datos)[1])
sub <- shuffle[1:dim(datos)[1] * 0.5]
datos[sub, 4] <- abs(datos[sub, 4]) * datos[sub, d+1]
datos[,d+1] <- factor(datos[,d+1])

datosB <- datos
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Ejercicio 2
#-------------------------------------------------------------------------------

# Apply all methods to a dataset
all.methods <- function(dataset){
  dims <- dim(dataset)[2]
  f <- dataset[,-dims]
  c <- dataset[,dims]

  FORW.rf  <- forward.ranking(f, c, method="rf.est", tot.trees=100, equalize.classes=FALSE)
  FORW.lda <- forward.ranking(f, c, method="lda.est")
  FORW.svm <- forward.ranking(f, c, method="svm.est")
  BACK.rf  <- backward.ranking(f, c, method="rf.est", tot.trees=100, equalize.classes=FALSE)
  BACK.lda <- backward.ranking(f, c, method="lda.est")
  BACK.svm <- backward.ranking(f, c, method="svm.est")
  KRUSKAL <- kruskal.filter(f, c)
  RFE.rf  <- rfe(f, c, method="imp.rf", tot.trees=100, equalize.classes=FALSE)
  RFE.svm <- rfe(f, c, method="imp.linsvm")

  cat("========================================\n")
  cat("FORWARD RF :=",  FORW.rf$ordered.features.list,"\n")
  cat("FORWARD LDA :=", FORW.lda$ordered.features.list,"\n")
  cat("FORWARD SVM :=", FORW.svm$ordered.features.list,"\n")
  cat("BACKWARD RF :=",  BACK.rf$ordered.features.list,"\n")
  cat("BACKWARD LDA :=", BACK.lda$ordered.features.list,"\n")
  cat("BACKWARD SVM :=", BACK.svm$ordered.features.list,"\n")
  cat("KRUSKAL :=", KRUSKAL$ordered.features.list,"\n")
  cat("RFE RF :=", RFE.rf$ordered.features.list,"\n")
  cat("RFE SVM :=", RFE.svm$ordered.features.list,"\n")
  cat("========================================\n")

}

## features_datosA <- all.methods(datosA)
## features_datosB <- all.methods(datosB)


#-------------------------------------------------------------------------------
# Ejercicio 3
#-------------------------------------------------------------------------------

# Genera datasets para el problema diagonal (definido en TP1)
gen_diagonal <- function(dims=2, size=1000, C=0.5) {

    sdev <- C * sqrt(dims)
    cov <- diag(dims, dims, x=sdev^2)

    size0 <- floor(size / 2)
    size1 <- size - size0

    data0 <- cbind(mvrnorm(n=size0, rep(-1, dims), cov), matrix( 0, size0, 1))
    data1 <- cbind(mvrnorm(n=size1, rep(+1, dims), cov), matrix(+1, size1, 1))

    frame <- as.data.frame(rbind(data0, data1))
    colnames(frame)[dims+1] <- "class"

    return(frame)
}


# Genera un dataset basado en el problema diagonal con con 10 variables reales y
# 90 variables de ruido.
gen_dataset <- function() {
  diagonal <- gen_diagonal(10, 100, 2/sqrt(10))

  real_features <- diagonal[,1:10]
  classes <- diagonal$class
  noise <- crea.ruido.unif(50, 90)[,1:90]

  dataset <- cbind(real_features, noise, classes)
  names(dataset) <- c(1:100, "class")

  return(dataset)
}


run_diagonal <- function() {

  FORW.rf <- FORW.lda <- FORW.svm <- rep(0, 30)
  BACK.rf <- BACK.lda <- BACK.svm <- rep(0, 30)
  KRUSKAL <- rep(0,30)
  RFE.rf <- RFE.svm <- rep(0,30)

  for (i in 1:30) {
    cat("----------------------------------------\n")
    cat("STEP :=", i, "\n")

    dataset <- gen_dataset()
    f <- dataset[,1:100]
    c <- factor(dataset$class)

    res <- forward.ranking(f, c, method="rf.est", tot.trees=100, equalize.classes=FALSE)
    FORW.rf[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- forward.ranking(f, c, method="lda.est")
    FORW.lda[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- forward.ranking(f, c, method="svm.est")
    FORW.svm[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- backward.ranking(f, c, method="rf.est", tot.trees=100, equalize.classes=FALSE)
    BACK.rf[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- backward.ranking(f, c, method="lda.est")
    BACK.lda[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- backward.ranking(f, c, method="svm.est")
    BACK.svm[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- kruskal.filter(f, c)
    KRUSKAL[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- rfe(f, c, method="imp.rf", tot.trees=100, equalize.classes=FALSE)
    RFE.rf[i] <- sum(res$ordered.features.list[1:10] <= 10)

    res <- rfe(f, c, method="imp.linsvm")
    RFE.svm[i] <- sum(res$ordered.features.list[1:10] <= 10)
    cat("n----------------------------------------\n")
  }

  cat("========================================\n")
  cat("FORWARD RF :=",  mean(FORW.rf)/10, "\n")
  cat("FORWARD LDA :=", mean(FORW.lda)/10, "\n")
  cat("FORWARD SVM :=", mean(FORW.svm)/10, "\n")
  cat("BACKWARD RF :=",  mean(BACK.rf)/10, "\n")
  cat("BACKWARD LDA :=", mean(BACK.lda)/10, "\n")
  cat("BACKWARD SVM :=", mean(BACK.svm)/10, "\n")
  cat("KRUSKAL :=", mean(KRUSKAL)/10, "\n")
  cat("RFE RF :=", mean(RFE.rf)/10, "\n")
  cat("RFE SVM :=", mean(RFE.svm)/10, "\n")
  cat("========================================\n")

}
