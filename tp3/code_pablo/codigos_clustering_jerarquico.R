library(stats)
library(cluster)

##dos clusters de dist. gaussianas
crea.gausianas<-function(tot.puntos=100,gap=2){
x<-rnorm(tot.puntos,mean=-gap)
y<-rnorm(tot.puntos,mean=0)
gausianas<-cbind(x,y,rep(1,length(x)))
x<-rnorm(tot.puntos,mean=gap)
y<-rnorm(tot.puntos,mean=0)
gausianas<-rbind(gausianas,cbind(x,y,rep(2,length(x))))
return(gausianas)
}

gausianas<-crea.gausianas()
plot(gausianas[,-3],pch=20,col=gausianas[,3],xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))

#ahora aplico hclust con single link
cc<-hclust(dist(gausianas[,-3]),method="single")
#controlar el dendograma
plot(cc)
#ver el clustering
plot(gausianas[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))

#repito con complete y average linkage
cc<-hclust(dist(gausianas[,-3]),method="complete")
#controlar el dendograma
plot(cc)
#ver el clustering
plot(gausianas[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))

cc<-hclust(dist(gausianas[,-3]),method="average")
#controlar el dendograma
plot(cc)
#ver el clustering
plot(gausianas[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))


##moons dataset
crea.moons<-function(tot.puntos=200,gap=-0.1,noise=0.07){
theta<-runif(tot.puntos,-pi/2+gap,pi/2-gap)
ro<-  rnorm(tot.puntos,1,noise)
x<-ro*cos(theta)-0.1
y<-ro*sin(theta)+0.5
moons<-cbind(x,y,rep(1,length(x)))
theta<-runif(tot.puntos,pi/2+gap,3/2*pi-gap)
ro<-  rnorm(tot.puntos,1,noise)
x<-ro*cos(theta)+0.1
y<-ro*sin(theta)-0.5
moons<-rbind(moons,cbind(x,y,rep(2,length(x))))
return(moons)
}

moons<-crea.moons()
plot(moons[,-3],pch=20,col=moons[,3],xlab="x",ylab="y",xlim=c(-2,2),ylim=c(-2,2))


#aplico hclust a moons
cc<-hclust(dist(moons[,-3]),method="single")
#controlar el dendograma
plot(cc)
#ver el clustering
plot(moons[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-2,2),ylim=c(-2,2))

cc<-hclust(dist(moons[,-3]),method="complete")
plot(cc)
plot(moons[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-2,2),ylim=c(-2,2))

cc<-hclust(dist(moons[,-3]),method="average")
plot(cc)
plot(moons[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-2,2),ylim=c(-2,2))


#aplico hclust average para obtener el dendograma de las ciudades de europa (eurodist) (ver ejemplo en codigo de mds)
cc<-hclust(eurodist,method="average")
# cc<-hclust(eurodist,method="single")
# cc<-hclust(eurodist,method="complete")
plot(cc)
#tambien puedo ver en el plot del mds como quedan los clusters, usando colores (6 clusters)
loc <- cmdscale(eurodist)
x <- loc[,1]
y <- -loc[,2]
plot(x, y, type="n", xlab="", ylab="", main="cmdscale(eurodist)")
text(x, y, rownames(loc), cex=0.8,col=cutree(cc,6))


#iris
data(iris)
#aplico average
cc<-hclust(dist(iris[,-5]),method="average")
#ploteo el dendrograma y marco 3 clusters, y ploteo el clustering
plot(cc)
rect.hclust(cc,3)

plot(iris[,-5],col=cutree(cc,k=3),pch=as.numeric(iris[,5]))
#repito con single
cc<-hclust(dist(iris[,-5]),method="single")
plot(iris[,-5],col=cutree(cc,k=3),pch=as.numeric(iris[,5]))



#genero un outlier en gausianas

viejo<-gausianas[1,]
gausianas[1,]<-c(-100,0,1)
plot(gausianas[,-3],pch=20,col=gausianas[,3],xlab="x",ylab="y")

gap<-3
#ahora aplico hclust con single link
cc<-hclust(dist(gausianas[,-3]),method="single")
#controlar el dendograma
plot(cc)
#ver el clustering con dos clusters
plot(gausianas[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-2*gap,2*gap),ylim=c(-2*gap,2*gap))
#ver el clustering con tres clusters (el outlier no destruye el resultado, hay que ignorarlo!)
plot(gausianas[,-3],pch=20,col=cutree(cc,k=3),xlab="x",ylab="y",xlim=c(-2*gap,2*gap),ylim=c(-2*gap,2*gap))

#repito con complete y average linkage
cc<-hclust(dist(gausianas[,-3]),method="complete")
#controlar el dendograma
plot(cc)
#ver el clustering
plot(gausianas[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-2*gap,2*gap),ylim=c(-2*gap,2*gap))

cc<-hclust(dist(gausianas[,-3]),method="average")
#controlar el dendograma
plot(cc)
#ver el clustering
plot(gausianas[,-3],pch=20,col=cutree(cc,k=2),xlab="x",ylab="y",xlim=c(-2*gap,2*gap),ylim=c(-2*gap,2*gap))

#vuelvo al dataset original
gausianas[1,]<-viejo


