library("adabag")
library("randomForest")
library("e1071")
library("dismo")

####################################
##                                ##
##          Ejercicio 1           ##
##                                ##
####################################

load("TP4.Rdata")

trees.number <- 200
trees.depths <- 1:20
boosting.runs <- 10

runBoosting <- function(train, test) {
  test.errors <- sapply(trees.depths, function (d) {
    runs <- replicate(boosting.runs, {
      predictor <- boosting(class~., data=train, mfinal=trees.number,
                            coef="Freund", control=rpart.control(maxdepth=d))
      return(predict.boosting(predictor, newdata=test)$error)
    })
    return(mean(runs))
  })
  return(test.errors)
}

## esp.boosting.error <- runBoosting(esp_train, esp_test)
## diag.boosting.error <- runBoosting(diag_train, diag_test)

## svg("esp.boosting.error.svg")
## plot(trees.depths, esp.boosting.error,
##      xlab="Maximum tree depth", ylab="Test error",
##      col="black", pch=20, lwd=2, ylim=c(0.3, 0.5))
## lines(lowess(trees.depths, esp.boosting.error, f=1/3),
##       col="red", lwd=2)

## svg("diag.boosting.error.svg")
## plot(trees.depths, diag.boosting.error,
##      xlab="Maximum tree depth", ylab="Test error",
##      col="black", pch=20, lwd=2, ylim=c(0.1,0.15))
## lines(lowess(trees.depths, diag.boosting.error, f=1/5),
##       col="red", lwd=2)

####################################
##                                ##
##          Ejercicio 2           ##
##                                ##
####################################
load("lampone.Rdata")
options(digits=2)

## PARAMETERS
nfolds <- 5                    # Number of folds
svm.costs <- 1*10^(-3:3)       # SVM C's
svm.rbf.gammas <- 1*10^(-3:3)  # SVM gammas
svm.poly.degrees <- 1:4        # SVM polynomial degrees
boosting.depths <- 1:20        # Boosting trees depths

## Prepare dataset
N_tipo  <- lampone[, 143]
lampone <- lampone[, -143]
lampone <- Filter(is.numeric, lampone) # This also filters the _anno_ column
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
lampone.svm <- function (C, k, ...) {
  folds.errors <- sapply(1:nfolds, function(fold) {
    test  <- lampone[lampone.folds == fold, ]
    train <- lampone[lampone.folds != fold, ]

    svm.obj <- svm(N_tipo~., train, cost=C, kernel=k,
                   scale = TRUE, type = 'C-classification', ...)
    pred <- predict(svm.obj, test)
    error <- sum(pred != test$N_tipo) / nrow(test)

    return(error)
  })
  return(mean(folds.errors))
}

lampone.svm.poly <- function () {
  error <- matrix(ncol=length(svm.costs), nrow=length(svm.poly.degrees))
  colnames(error) <- svm.costs
  rownames(error) <- svm.poly.degrees

  for (d in svm.poly.degrees) {
    for (C in svm.costs) {
    cat("SVM poly [ C :=", C, "]\t[ degree :=", d, "]\n")
    error[toString(d), toString(C)] <- lampone.svm(C, "polynomial", degree=d, gamma=1)
    }
  }

  return(error)
}

lampone.svm.rbf <- function () {
  error <- matrix(ncol=length(svm.costs), nrow=length(svm.rbf.gammas))
  colnames(error) <- svm.costs
  rownames(error) <- svm.rbf.gammas

  for(g in svm.gammas)
    for (C in svm.costs) {
      cat("SVM radial [ C :=", C, "]\t[ gamma :=", g, "]\n")
      error[toString(g), toString(C)] <- lampone.svm(C, "radial", gamma=g)
    }

  return(error)
}

## lampone.svm.poly.error <- lampone.svm.poly()
## lampone.svm.rbf.error <- lampone.svm.rbf()

## svg("lampone.svm.poly.error.svg")
## plot(-3:3, lampone.svm.poly.error[1,],
##      xlab="C", ylab="5-fold cross validation error",
##      col="red", pch=20, lwd=2, ylim=c(0, 0.3), type="o", xaxt='n')
## lines(-3:3, lampone.svm.poly.error[2,],
##      col="green", pch=20, lwd=2, type="o")
## lines(-3:3, lampone.svm.poly.error[3,],
##      col="blue", pch=20, lwd=2, type="o")
## lines(-3:3, lampone.svm.poly.error[4,],
##      col="magenta", pch=20, lwd=2, type="o")
## axis(side=1, at=-3:3, sapply(svm.costs, toString))
## legend("topright", title="Degree", legend=1:4,
##        col=c("red", "green", "blue", "magenta"), lwd=2)

## svg("lampone.svm.rbf.error.svg")
## plot(-3:3, lampone.svm.rbf.error[1,],
##      xlab="C", ylab="5-fold cross validation error",
##      col="red", pch=20, lwd=2, ylim=c(0.2, 0.55), type="o", xaxt='n')
## lines(-3:3, lampone.svm.rbf.error[2,],
##      col="green", pch=20, lwd=2, type="o")
## lines(-3:3, lampone.svm.rbf.error[3,],
##      col="blue", pch=20, lwd=2, type="o")
## axis(side=1, at=-3:3, sapply(svm.costs, toString))
## legend("topright", title="Gamma", legend=c("0.001", "0.01", ">=0.1"),
##        col=c("red", "green", "blue"), lwd=2)

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
