########################################
#           GAP STATISTIC              #
########################################
# method : {kmeans | hclust.{s|c|a}    #
# K : max number of clusters           #
# B : number of reference datasets     #
########################################
gapStatistic <- function(dataset, method, K=10, B=5) {

  runMethod <- function(d, k)
    switch(method
    , "kmeans"   = kmeans(d, k, nstart=10)$tot.withinss
    , "hclust.s" = hc(d, k, "single")
    , "hclust.c" = hc(d, k, "complete")
    , "hclust.a" = hc(d, k, "average"))

  hc <- function(d, k, l) {
    clusters <- cutree(hclust(dist(d), l), k)
    clusters.dists <- sapply(1:k, function(k) {
      cluster.k <- clusters[clusters == k]
      if(length(cluster.k) > 0) return (sum(dist(d[clusters == k, ])))
      return(0)
    })
    return(sum(clusters.dists))
  }

  data <- prcomp(dataset)$x
  data.min <- apply(data, 2, min)
  data.max <- apply(data, 2, max)

  Wk <- sapply(1:K, function(k) runMethod(data, k))

  Wkb <- matrix(nrow=K, ncol=B)

  for (k in 1:K)
    Wkb[k,] <- replicate(B, runMethod(runifv(data.min, data.max, nrow(data)),k))

  l <- rowSums(log(Wkb)) / B
  gap <- l - log(Wk)

  s <- sapply(1:K, function(k) {
    sqrt(sum((log(Wkb[k,]) - l[k]) ** 2) / B) * sqrt((1 + 1 / B))
  })

  for (k in 1:(K-1)) if(gap[k] >= gap[k+1] - s[k+1]) return(k)

  return(NA)
}

## runif variant for random vectors within in n-dimensional boxes given by min
## and max vectors.
runifv <- function(min, max, n) {
  sapply(1:length(min), function(d) runif(min=min[d], max=max[d], n))
}

########################################
#              STABILITY               #
########################################
# method : {kmeans | hclust.{s|c|a}    #
# K : max number of clusters           #
# B : number of reference datasets     #
########################################
stability <- function(dataset, method, K, B, ratio=0.8) {

  runMethod <- function(d, k)
    switch(method
    , "kmeans"   = kmeans(d, k, nstart=10)$cluster
    , "hclust.s" = cutree(hclust(dist(dataset),"single"),k)
    , "hclust.c" = cutree(hclust(dist(dataset),"complete"),k)
    , "hclust.a" = cutree(hclust(dist(dataset),"average"),k))

  combs <- combn(B,2)
  scores <- matrix(nrow=K, ncol=ncol(combs))

  dataset.size <- nrow(dataset)
  subsample.size <- dataset.size * ratio

  indexes <- matrix(nrow=B, ncol=subsample.size)

  for (b in 1:B) indexes[b,] <- sample(dataset.size, subsample.size)

  for (k in 1:K)
    for (comb in 1:ncol(combs)) {

      i1 <- indexes[combs[1, comb], ]
      i2 <- indexes[combs[2, comb], ]

      v1 <- v2 <- rep(0, dataset.size)
      v1[i1] <- runMethod(dataset[i1, ], k)
      v2[i2] <- runMethod(dataset[i2, ], k)

      a <- sqrt(v1 %*% t(v1))
      m1 <- a / -a + 2 * (a == round(a))
      m1[is.nan(m1)] <- 0

      a <- sqrt(v2 %*% t(v2))
      m2 <- a / -a + 2 * (a == round(a))
      m2[is.nan(m2)] <- 0

      val <- sum(v1 * v2 > 0)
      scores[k, comb] <- sum((m1*m2)[upper.tri(m1)] > 0) / (val * (val-1) / 2)
    }

  return(scores)
}
