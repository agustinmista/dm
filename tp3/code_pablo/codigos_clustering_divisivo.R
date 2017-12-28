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
plot(gausianas[,-3],pch=19+gausianas[,3],xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))

#aplico k-means
cc<-kmeans(gausianas[,-3],cent=2)
plot(gausianas[,-3],pch=19+gausianas[,3],col=cc$cluster,xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))
abline(v=0)

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

#aplico k-means - QUE DEBERIA DAR???
cc<-kmeans(moons[,-3],cent=2)
plot(moons[,-3],pch=20,col=cc$cluster,xlab="x",ylab="y",xlim=c(-2,2),ylim=c(-2,2))

##genero un outlier en gausianas
viejo<-gausianas[1,]
gausianas[1,]<-c(-100,0,1)
plot(gausianas[,-3],pch=20,col=gausianas[,3],xlab="x",ylab="y")
#aplico k-means
cc<-kmeans(gausianas[,-3],cent=2,iter=100)
plot(gausianas[,-3],pch=20,col=cc$cluster,xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))
abline(v=0)

#ahora aplico PAM
cc<-pam(gausianas[,-3],k=2,cluster.only=T)
plot(gausianas[,-3],pch=20,col=cc,xlab="x",ylab="y",xlim=c(-5,5),ylim=c(-5,5))
abline(v=0)

#vuelvo al dataset original
gausianas[1,]<-viejo


##iris - como demo. Aplico kmeans k=3 y plot (probar varias veces, a veces da minimo local, se soluciona con nstart=10)
data(iris)
cc<-kmeans(iris[,-5],cent=3)
plot(iris[,-5],col=cc$cluster,pch=as.numeric(iris[,5]))
