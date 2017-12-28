load("spam.Rdata")

########################################
##       DATASET PREPROCESSING        ##
########################################
spam.class <- factor(spam[,58])

spam.pca <- prcomp(scale(log(spam[,-58]+1)))

## o = original; l = log; s = scale; p = PCA
spam.feats <- list(
  o   = data.frame(spam[,-58]),
  l   = data.frame(log(spam[,-58]+1)),
  ls  = data.frame(scale(log(spam[,-58]+1))),
  lsp = data.frame(spam.pca$x)
)

########################################
##       DATASET VISUALIZATION        ##
########################################

library(ggfortify)

## PCA variances
## svg("spam.pca.variances.svg")
## plot(spam.pca)

## PCA Biplot
## spam$class <- factor(spam.class, labels=c("ham", "spam"))

## svg("spam.pca.loadings.svg")
## autoplot(spam.pca, loadings=T, loadings.label=T, data=spam, colour='class',
##          loadings.label.colour="black",
##          loadings.colour="black") + coord_fixed(ratio=1)

## autoplot(spam.pca, data=spam, colour=spam.km$o)
## autoplot(spam.pca, data=spam, colour=spam.km$l)
## autoplot(spam.pca, data=spam, colour=spam.km$lsp)

########################################
##         FEATURES SELECTION         ##
########################################
source("feature-selection.R")

## feat.sel <- feature.selection.all(spam.feats$ls, spam.class)

########################################
##            CLUSTERING              ##
########################################

source("clustering.R")

## spam.km  <- data.frame(sapply(spam.feats, km))
## spam.hcs <- data.frame(sapply(spam.feats, hcs))
## spam.hcc <- data.frame(sapply(spam.feats, hcc))
## spam.hca <- data.frame(sapply(spam.feats, hca))

## matchIsSpam <- function(c) clusterMatches(spam.class, c)
## spam.km.matches  <- sapply(spam.km,  matchIsSpam)
## spam.hcs.matches <- sapply(spam.hcs, matchIsSpam)
## spam.hcc.matches <- sapply(spam.hcc, matchIsSpam)
## spam.hca.matches <- sapply(spam.hca, matchIsSpam)

########################################
##          CLASSIFICATION            ##
########################################

library("adabag")
library("randomForest")
library("e1071")
library("dismo")

nfolds <- 10                  # Number of folds
svm.costs <- 1*10^(-3:3)      # SVM C's
svm.rbf.gammas <- 1*10^(-3:3) # SVM gammas
svm.poly.degrees <- 1:4       # SVM polynomial degrees
boosting.depths <- 1:20       # Boosting trees depths

spam.folds <- kfold(spam, 10, by=spam.class)

########################################

## Run Random Forest method over each fold and take the mean error
spam.rf <- function() {
  cat("Random Forest\n")

  folds.errors <- sapply(1:nfolds, function(fold) {

    test  <- cbind(spam.feats$lsp, spam.class)[spam.folds == fold, ]
    train.feats <- spam.feats$lsp[spam.folds != fold, ]
    train.class <- spam.class[spam.folds != fold]

    pred <- predict(randomForest(train.feats, train.class), test)
    error <- sum(pred != spam.class) / nrow(test)
    return(error)
  })

  return(mean(folds.errors))
}

## spam.rf.error <- spam.rf()

########################################

## Run boosting method over each fold and take the mean error
spam.boosting <- function(d) {
  cat("Boosting [ depth :=", d, "]\n")

  folds.errors <- sapply(1:nfolds, function(fold) {

    test  <- cbind(spam.feats$lsp, spam.class)[spam.folds == fold, ]
    train <- cbind(spam.feats$lsp, spam.class)[spam.folds != fold, ]

    boosting.obj <- boosting(spam.class~., train, mfinal=100,
                             coef="Freund", control=rpart.control(maxdepth=d))
    pred <- predict.boosting(boosting.obj, newdata=test)
    error <- pred$error

    return(error)
  })

  return(mean(folds.errors))
}

## spam.boosting.error <- sapply(boosting.depths, spam.boosting)

## svg("spam.boosting.error.svg")
## plot(boosting.depths, spam.boosting.error,
##      ylab="10-fold cross validation error", xlab="Maximum tree depth",
##      type="o", col="red", pch=20, lwd=2, ylim=c(0,0.1))

########################################

## Run SVM method over each fold and take the mean error
spam.svm <- function (C, k, ...) {

  folds.errors <- sapply(1:nfolds, function(fold) {

    test  <- cbind(spam.feats$lsp, spam.class)[spam.folds == fold, ]
    train <- cbind(spam.feats$lsp, spam.class)[spam.folds != fold, ]

    svm.obj <- svm(spam.class~., train, cost=C, kernel=k,
                   scale = TRUE, type = 'C-classification', ...)
    pred <- predict(svm.obj, test)
    error <- sum(pred != test$spam.class) / nrow(test)

    return(error)
  })

  return(mean(folds.errors))
}

spam.svm.poly <- function () {
  error <- matrix(ncol=length(svm.costs), nrow=length(svm.poly.degrees))
  colnames(error) <- svm.costs
  rownames(error) <- svm.poly.degrees

  for (d in svm.poly.degrees) {
    for (C in svm.costs) {
    cat("SVM poly [ C :=", C, "]\t[ degree :=", d, "]\n")
    error[toString(d), toString(C)] <- spam.svm(C, "polynomial", degree=d, gamma=1)
    }
  }

  return(error)
}

spam.svm.rbf <- function () {
  error <- matrix(ncol=length(svm.costs), nrow=length(svm.rbf.gammas))
  colnames(error) <- svm.costs
  rownames(error) <- svm.rbf.gammas

  for(g in svm.gammas)
    for (C in svm.costs) {
      cat("SVM radial [ C :=", C, "]\t[ gamma :=", g, "]\n")
      error[toString(g), toString(C)] <- spam.svm(C, "radial", gamma=g)
    }

  return(error)
}

## spam.svm.poly.error <- spam.svm.poly()
## spam.svm.rbf.error <- spam.svm.rbf()

## svg("spam.svm.poly.error.svg")
## plot(-3:3, spam.svm.poly.error[1,],
##      xlab="C", ylab="10-fold cross validation error",
##      col="red", pch=20, lwd=2, ylim=c(0, 0.2), type="o", xaxt='n')
## lines(-3:3, spam.svm.poly.error[2,],
##      col="green", pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.poly.error[3,],
##      col="blue", pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.poly.error[4,],
##      col="magenta", pch=20, lwd=2, type="o")
## axis(side=1, at=-3:3, sapply(svm.costs, toString))
## legend("topright", title="Degree", legend=1:4,
##        col=c("red", "green", "blue", "magenta"), lwd=2)

## svg("spam.svm.rbf.error.svg")
## plot(-3:3, spam.svm.rbf.error[1,],
##      xlab="C", ylab="10-fold cross validation error",
##      col=1, pch=20, lwd=2, ylim=c(0, 0.5), type="o", xaxt='n')
## lines(-3:3, spam.svm.rbf.error[2,],
##      col=2, pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.rbf.error[3,],
##      col=3, pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.rbf.error[4,],
##      col=4, pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.rbf.error[5,],
##      col=5, pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.rbf.error[6,],
##      col=6, pch=20, lwd=2, type="o")
## lines(-3:3, spam.svm.rbf.error[7,],
##      col=7, pch=20, lwd=2, type="o")
## axis(side=1, at=-3:3, sapply(svm.costs, toString))
## legend("topright", title="Gamma", legend=sapply(svm.rbf.gammas, toString),
##        col=1:7, lwd=2)
