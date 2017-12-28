library(densityClust)

gaussians <- function() {
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

noise <- function() {
  sapply(1:2, function(d) runif(min=-6, max=8, 50))
}

# Genero el dataset.
g <- gaussians()
plot(g[,1:2])

n <- noise()
plot(n)

dataset <- rbind(g[,1:2], n)

plot(dataset)

# Calculo los atributos de clustering usando el algoritmo del paper.
dc <- densityClust(dataset)
plot(dc)

# Nos permite elegir manualmente el umbral de corte (d_C).
clusters <- findClusters(dc, rho=0.7, delta=2.7)

# Muestra los datos clusterizados.
plotMDS(clusters)
