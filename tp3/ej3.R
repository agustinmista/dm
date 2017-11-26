source("ej1.R")
source("ej2.R")
data(iris)


four_gaussians <- function() {

  tot <- 100
  gap <- 2

  x <- rnorm(tot, mean=-gap)
  y <- rnorm(tot, mean=-gap)
  data <- cbind(x, y, rep(1, length(x)))

  x <- rnorm(tot, mean=2*gap)
  y <- rnorm(tot, mean=0)
  data <- rbind(data, cbind(x, y, rep(2, length(x))))

  x <- rnorm(tot, mean=0.7*gap, sd=0.5)
  y <- rnorm(tot, mean=2.5*gap, sd=0.5)
  data <- rbind(data, cbind(x, y, rep(3, length(x))))

  x <- rnorm(tot, mean=-gap, sd=0.5)
  y <- rnorm(tot, mean=gap, sd=0.5)
  data<-rbind(data, cbind(x, y, rep(4, length(x))))

  return(data)
}

gaussianas <- four_gaussians()


########################################
#                TESTS                 #
########################################

## Gap Statistic
runGapStatistic <- function() {

  N <- 25
  methods <- c("kmeans", "hclust.s", "hclust.c", "hclust.a")
  gs <- gapStatistic

  stats.clusters <- sapply (methods, function(m) {
    list(
      gaussianas = median(replicate(N, gs(gaussianas[, 1:2], m, 10, 10))),
      iris       = median(replicate(N, gs(iris[, 1:4], m, 10, 10))),
      iris.ls    = median(replicate(N, gs(scale(log(iris[,1:4])), m, 10, 10))),
      lampone    = median(replicate(N, gs(scale(log(iris[, 1:4])), m, 10, 10))),
      lampone.ls = median(replicate(N, gs(scale(log(lampone)), m, 10, 10)))
    )})

  return(stats.clusters)
}

## gapStatistic.test <- runGapStatistic()


## Stability
runStability <- function() {
  methods <- c("kmeans", "hclust.s", "hclust.c", "hclust.a")
  st <- stability

  stats.stability <- sapply (methods, function(m) {
    list(
      gaussianas    = st(gaussianas[, 1:2], m, 10, 25),
      iris          = st(iris[, 1:4], m, 10, 25),
      iris.ls       = st(lampone, m, 10, 25),
      lampone       = st(scale(log(iris[, 1:4]) + 1), m, 10, 25),
      lampone.ls    = st(scale(log(lampone)), m, 10, 25)
    )})

  return(stats.stability)
}

## stability.test <- runStability()

plotScores <- function(scores){

  X <- Y <- data.frame(scores)
  combs.n <- ncol(X)
  K <- nrow(X)

  for(i in 1:K){
    X[i, ] <- sort(X[i,])
    Y[i, ] <- cumsum(1:combs.n)
    Y[i, ] <- Y[i, ] / max(Y[i, ])
  }

  plot( 0, 0
      , xlim=c(0,1)
      , ylim=c(0,1)
      , xlab="Score", ylab="Acumulado" )

  for(i in 1:K) lines(X[i, ], Y[i, ], col=i, type="l" )

  legend( x="topleft"
        , legend=1:K
        , col=1:10
        , lwd=2
        )
}
