
library(longclust)
library(e1071)
rm(list=ls())
mat <- matrix(c(1,2,3,4,1,6,2,5,1,3,8,10),4)
mat

?matplot

#xaxt supresses the x-axis labelling
matplot(t(mat),typ="l",lty=1,col=1,xlab="Time",ylab="Expression",xaxt="n")
axis(1,at=c(1:3))

#try u=1.5, 1, 2
SEED<-827
set.seed(SEED)
mat <- matrix(c(1,2,3,4,1,6,2,5,1,3,8,10),4)
u<-1.5 # This controls the "difficulty"
for(i in 1:99){
	mat_temp <- matrix(c(1+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),4+runif(1,-u,u),1+runif(1,-u,u),6+runif(1,-u,u),2+runif(1,-u,u),5+runif(1,-u,u),1+runif(1,-u,u),3+runif(1,-u,u),8+runif(1,-u,u),10+runif(1,-u,u)),4)
	mat<-t(cbind(t(mat),t(mat_temp)))
}
matplot(t(mat),typ="l",lty=1,col=1,xlab="Time",ylab="Expression",xaxt="n")
axis(1,at=c(1:3))

?longclustEM
long_res<-longclustEM(mat,2,6,gaussian=TRUE)
N<-dim(mat)[1]
map<-rep(0,N)
for(i in 1:N){
	map[i]<-which(long_res$zbest[i,]==max(long_res$zbest[i,]))
}

matplot(t(mat),typ="l",col=map,lty=1,xlab="Time",ylab="Expression",xaxt="n")
axis(1,at=c(1:3))
table(rep(c(1:4),N/4),map)
classAgreement(table(rep(c(1:4),N/4),map))

# Five times, harder
mat <- matrix(c(1,2,3,4,1,4,2,5,2,3,3,4,5,2,4,5,4,2,3,5),4)
mat

matplot(t(mat),typ="l",col=1)

#try u=1.15, 1, 2
set.seed(SEED+2)
u<-1.15
for(i in 1:99){
	mat_temp <-matrix(c(1+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),4+runif(1,-u,u),1+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),5+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),3+runif(1,-u,u),4+runif(1,-u,u),5+runif(1,-u,u),2+runif(1,-u,u),4+runif(1,-u,u),5+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),5+runif(1,-u,u)),4)
	mat<-t(cbind(t(mat),t(mat_temp)))
}
matplot(t(mat),typ="l",col=1,lty=1,xlab="Time",ylab="Expression")

long_res<-longclustEM(mat,2,6,gaussian=TRUE)
N<-dim(mat)[1]
map<-rep(0,N)
for(i in 1:N){
	map[i]<-which(long_res$zbest[i,]==max(long_res$zbest[i,]))
}
matplot(t(mat),typ="l",col=map,lty=1,xlab="Time",ylab="Expression")

table(rep(c(1:4),N/4),map)
classAgreement(table(rep(c(1:4),N/4),map))

# Eight times, harder
set.seed(SEED)
mat <- matrix(c(2,3,4,4,2,4,2,3,4,4,2,3,2,3,3,4,2,2,2,3,2,4,2,1),3)
mat

matplot(t(mat),typ="l",col=1)

u<-1.5 # For u<-2.5 this is very hard
for(i in 1:99){
	mat_temp <-matrix(c(2+runif(1,-u,u),3+runif(1,-u,u),4+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),4+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),3+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),2+runif(1,-u,u),2+runif(1,-u,u),3+runif(1,-u,u),2+runif(1,-u,u),4+runif(1,-u,u),2+runif(1,-u,u),1+runif(1,-u,u)),3)
	mat<-t(cbind(t(mat),t(mat_temp)))
}
matplot(t(mat),typ="l",col=1,lty=1,xlab="Time",ylab="Expression")

long_res<-longclustEM(mat,3,3,gaussian=TRUE)
N<-300
map<-rep(0,N)
for(i in 1:N){
	map[i]<-which(long_res$zbest[i,]==max(long_res$zbest[i,]))
}
matplot(t(mat),typ="l",col=map,lty=1,xlab="Time",ylab="Expression")

tab<-table(rep(c(1:3),N/3),map)
tab

tab[,c(3,1,2)]

classAgreement(tab[,c(3,1,2)])