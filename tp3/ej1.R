library(MASS)
library(e1071)
library(cluster)


########################################
#               METHODS                #
########################################
kmeans.iters = 100

## km = kmeans; hc{s|c|a} = hclust {single|complete|average};
km  <- function(dataset) { kmeans(dataset, 2, kmeans.iters, nstart=10)$cluster }
hcs <- function(dataset) { cutree(hclust(dist(dataset), "single"), k=2) }
hcc <- function(dataset) { cutree(hclust(dist(dataset), "complete"), k=2) }
hca <- function(dataset) { cutree(hclust(dist(dataset), "average"), k=2) }

########################################
#              ANALYSIS                #
########################################

## Returns matching percentage between a class vector and a clustering.
clusterMatches <- function(class, clusters) {
  return(compareMatchedClasses(class, clusters, method="exact")$diag)
}

compareClusters <- function(c1, c2) {
  cont.table <- table(c1, c2)
  class.match <- matchClasses(as.matrix(cont.table), method="exact")
  print(cont.table[,class.match])
}

########################################
#                CRABS                 #
########################################
data(crabs)

## Classes
crabs.species <- crabs[,1]
crabs.sex     <- crabs[,2]

## Preprocessing
## o = original; l = log; s = scale; p = PCA
crabs.features <- list(
  o   = crabs[,4:8],
  l   = log(crabs[,4:8]),
  ls  = data.frame(scale(log(crabs[,4:8]))),
  lp  = data.frame(prcomp(log(crabs[,4:8]))$x),
  lsp = data.frame(prcomp(scale(log(crabs[,4:8])))$x),
  lps = data.frame(scale(prcomp(log(crabs[,4:8]))$x))
)

## Clustering
crabs.km  <- data.frame(sapply(crabs.features, km))
crabs.hcs <- data.frame(sapply(crabs.features, hcs))
crabs.hcc <- data.frame(sapply(crabs.features, hcc))
crabs.hca <- data.frame(sapply(crabs.features, hca))

## Match Percentage
matchCrabSpecies <- function(c) { clusterMatches(crabs.species, c) }
species.km  <- sapply(crabs.km,  matchCrabSpecies)
species.hcs <- sapply(crabs.hcs, matchCrabSpecies)
species.hcc <- sapply(crabs.hcc, matchCrabSpecies)
species.hca <- sapply(crabs.hca, matchCrabSpecies)

matchCrabsSex <- function(c) clusterMatches(crabs.sex, c)
sex.km  <- sapply(crabs.km,  matchCrabsSex)
sex.hcs <- sapply(crabs.hcs, matchCrabsSex)
sex.hcc <- sapply(crabs.hcc, matchCrabsSex)
sex.hca <- sapply(crabs.hca, matchCrabsSex)

crabs.matches <- rbind(species.km, species.hcs, species.hcc, species.hca,
                       sex.km, sex.hcs, sex.hcc, sex.hca)

########################################
#               LAMPONE                #
########################################
load("lampone.Rdata")

## Classes
lampone.year    <- lampone[,1]
lampone.species <- lampone[,143]

## Sanitizing dataset
lampone <- lampone[,-c(1,143)]                     # remove classes cols
lampone <- Filter(is.numeric, lampone)             # filter non-numeric cols
lampone <- Filter(function(x) sd(x) != 0, lampone) # filter constant cols
lampone <- lampone + 1                             # avoid log(0) errors

## Preprocessing
## o = original; l = log; s = scale; p = PCA
lampone.features <- list(
  o   = lampone,
  l   = log(lampone),
  ls  = data.frame(scale(log(lampone))),
  lp  = data.frame(prcomp(log(lampone))$x),
  lsp = data.frame(prcomp(scale(log(lampone)))$x),
  lps = data.frame(scale(prcomp(log(lampone))$x))
)

## Clustering
lampone.km  <- data.frame(sapply(lampone.features, km))
lampone.hcs <- data.frame(sapply(lampone.features, hcs))
lampone.hcc <- data.frame(sapply(lampone.features, hcc))
lampone.hca <- data.frame(sapply(lampone.features, hca))

## Match Percentage
matchLamponeSpecies <- function(c) { clusterMatches(lampone.species, c) }
species.km  <- sapply(lampone.km,  matchLamponeSpecies)
species.hcs <- sapply(lampone.hcs, matchLamponeSpecies)
species.hcc <- sapply(lampone.hcc, matchLamponeSpecies)
species.hca <- sapply(lampone.hca, matchLamponeSpecies)

matchLamponeYear <- function(c) clusterMatches(lampone.year, c)
year.km  <- sapply(lampone.km,  matchLamponeYear)
year.hcs <- sapply(lampone.hcs, matchLamponeYear)
year.hcc <- sapply(lampone.hcc, matchLamponeYear)
year.hca <- sapply(lampone.hca, matchLamponeYear)

lampone.matches <- rbind(species.km, species.hcs, species.hcc, species.hca,
                         year.km, year.hcs, year.hcc, year.hca)
