library(randomForest)
library(kernlab)
library(MASS)


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
rf.est <- function(x.train, y, tot.trees=500, mtry=0) {
	if (mtry < 1) mtry <- floor(sqrt(dim(x.train)[2]))
	return(randomForest(x.train, y, mtry=mtry,ntree=tot.trees,)$err.rate[tot.trees])
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
imp.rf <- function(x.train, y, tot.trees=500, mtry=0) {
	if (mtry < 1) mtry <- floor(sqrt(dim(x.train)[2]))
	m.rf <- randomForest(x.train, y, ntree=tot.trees, mtry=mtry, importance=TRUE)
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


####################################
## Apply all methods to a dataset ##
####################################
feature.selection.all <- function(f, c){
  
  cat("========================================\n")

  FORW.rf  <- forward.ranking(f, c, method="rf.est", tot.trees=100)
  cat("FORWARD RF :=",  FORW.rf$ordered.features.list,"\n")

  FORW.lda <- forward.ranking(f, c, method="lda.est")
  cat("FORWARD LDA :=", FORW.lda$ordered.features.list,"\n")

  FORW.svm <- forward.ranking(f, c, method="svm.est")
  cat("FORWARD SVM :=", FORW.svm$ordered.features.list,"\n")

  BACK.rf  <- backward.ranking(f, c, method="rf.est", tot.trees=100)
  cat("BACKWARD RF :=",  BACK.rf$ordered.features.list,"\n")

  BACK.lda <- backward.ranking(f, c, method="lda.est")
  cat("BACKWARD LDA :=", BACK.lda$ordered.features.list,"\n")

  BACK.svm <- backward.ranking(f, c, method="svm.est")
  cat("BACKWARD SVM :=", BACK.svm$ordered.features.list,"\n")

  KRUSKAL <- kruskal.filter(f, c)
  cat("KRUSKAL :=", KRUSKAL$ordered.features.list,"\n")

  RFE.rf  <- rfe(f, c, method="imp.rf", tot.trees=100)
  cat("RFE RF :=", RFE.rf$ordered.features.list,"\n")

  RFE.svm <- rfe(f, c, method="imp.linsvm")
  cat("RFE SVM :=", RFE.svm$ordered.features.list,"\n")

  cat("========================================\n")

  return(list(
    FORW.rf  = FORW.rf$ordered.features.list,
    FORW.lda = FORW.lda$ordered.features.list,
    FORW.svm = FORW.svm$ordered.features.list,
    BACK.rf  = BACK.rf$ordered.features.list,
    BACK.lda = BACK.lda$ordered.features.list,
    BACK.svm = BACK.svm$ordered.features.list,
    KRUSKAL  = KRUSKAL$ordered.features.list,
    RFE.rf   = RFE.rf$ordered.features.list,
    RFE.svm  = RFE.svm$ordered.features.list
  ))
}
