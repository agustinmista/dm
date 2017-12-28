library("adabag")
library("randomForest")
library("e1071")
library("dismo")

## PARAMETERS
nfolds <- 5                 # Number of folds
svm.costs <- 1*10^(-3:3)    # SVM C's
svm.gammas <- 1*10^(-3:3)   # SVM gammas
svm.poly.degrees <- 1:4     # SVM polynomial degrees
boosting.depths <- 1:20     # Boosting trees depths

## Prepare dataset
N_tipo  <- lampone[, 143]
lampone <- lampone[, -143]
lampone <- Filter(is.numeric, lampone)
lampone <- Filter(function(x) sd(x) != 0, lampone)
lampone <- cbind(lampone, N_tipo)

## Create folds
## lampone.folds <- kfold(lampone, nfolds, by=N_tipo)

## --------------- ##
##  RANDOM FOREST  ##
## --------------- ##

## Run Random Forest method over each fold and take the mean error
lampone.rf <- function() {
  cat("Random Forest\n")

  folds.errors <- sapply(1:nfolds, function(fold) {

    test  <- lampone[lampone.folds == fold, ]
    train <- lampone[lampone.folds != fold, ]

    pred <- predict(randomForest(N_tipo~., train), test)
    error <- sum(pred != test$N_tipo) / nrow(test)
    return(error)
  })

  return(mean(folds.errors))
}

## lampone.rf.error <- lampone.rf()

## -------------- ##
##      SVM       ##
## -------------- ##

# Run SVM method over each fold and take the mean error
lampone.svm <- function (g, C, k, ...) {
  folds.errors <- sapply(1:nfolds, function(fold) {
    test  <- lampone[lampone.folds == fold, ]
    train <- lampone[lampone.folds != fold, ]

    svm.obj <- svm(N_tipo~., train, gamma=g, C=C, kernel=k,
                   scale = TRUE, type = 'C-classification', ...)
    pred <- predict(svm.obj, test)
    error <- sum(pred != test$N_tipo) / nrow(test)

    return(error)
  })
  return(mean(folds.errors))
}

lampone.svm.poly <- function (d) {
  error <- matrix(ncol=length(svm.costs), nrow=length(svm.gammas))
  colnames(error) <- svm.costs
  rownames(error) <- svm.gammas

  for(g in svm.gammas)
    for (C in svm.costs) {
      cat("SVM poly [ gamma :=", g, "]\t[ C :=", C, "]\t[ degree :=", d, "]\n")
      error[toString(g), toString(C)] <- lampone.svm(g, C, "polynomial", degree=d)
    }

  return(error)
}



lampone.svm.rbf <- function () {
  error <- matrix(ncol=length(svm.costs), nrow=length(svm.gammas))
  colnames(error) <- svm.costs
  rownames(error) <- svm.gammas

  for(g in svm.gammas)
    for (C in svm.costs) {
      cat("SVM radial [ gamma :=", g, "]\t[ C :=", C, "]\n")
      error[toString(g), toString(C)] <- lampone.svm(g, C, "radial")
    }

  return(error)
}

## lampone.svm.poly.error <- lapply(svm.poly.degrees, lampone.svm.poly)
## lampone.svm.rbf.error <- lampone.svm.rbf()

## svg("lampone.svm.poly.error.svg")
## plot(-3:3, (lampone.svm.poly.error[[1]])[,1],
##      xlab="Gamma", ylab="5-fold cross validation error",
##      col="red", pch=20, lwd=2, ylim=c(0, 0.3), type="o", xaxt='n')
## lines(-3:3, (lampone.svm.poly.error[[2]])[,1],
##      col="green", pch=20, lwd=2, type="o")
## lines(-3:3, (lampone.svm.poly.error[[3]])[,1],
##      col="blue", pch=20, lwd=2, type="o")
## lines(-3:3, (lampone.svm.poly.error[[4]])[,1],
##      col="magenta", pch=20, lwd=2, type="o")
## axis(side=1, at=-3:3, sapply(svm.gammas, toString))
## legend("topright", legend=1:4, col=c("red", "green", "blue", "magenta"), lwd=2)

## svg("lampone.svm.rbf.error.svg")
## plot(-3:3, lampone.svm.rbf.error[,1],
##      xlab="Gamma", ylab="5-fold cross validation error",
##      col="orange", pch=20, lwd=2, ylim=c(0, 0.5), type="o", xaxt='n')
## axis(side=1, at=-3:3, sapply(svm.gammas, toString))


## -------------- ##
##    ADABOOST    ##
## -------------- ##

# Run boosting method over each fold and take the mean error
lampone.boosting <- function(d) {
  cat("Boosting [ depth :=", d, "]\n")

  folds.errors <- sapply(1:nfolds, function(fold) {

    test  <- lampone[lampone.folds == fold, ]
    train <- lampone[lampone.folds != fold, ]

    boosting.obj <- boosting(N_tipo~., data=train, mfinal=100,
                             coef="Freund", control=rpart.control(maxdepth=d))
    pred <- predict.boosting(boosting.obj, newdata=test)
    error <- pred$error

    return(error)
  })

  return(mean(folds.errors))
}

## lampone.boosting.error <- sapply(boosting.depths, lampone.boosting)
## svg("lampone.boosting.error.svg")
## plot(trees.depths, lampone.boosting.error,
##      ylab="5-fold cross validation error", xlab="Maximum tree depth",
##      col="black", pch=20, lwd=2, ylim=c(0,0.15))
## lines(lowess(trees.depths, lampone.boosting.error, f=1/2),
##       col="red", lwd=2)
