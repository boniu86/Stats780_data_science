rm(list=ls())
library("vscc")
library(clustvarsel)

# Coffee data
data(coffee,package="pgmm")
#pairs(coffee[,-c(1,2)])
pairs(coffee[,-c(1,2)],col=coffee[,1])
x<-scale(coffee[,-c(1,2)])
run<-vscc(x)
run

table(coffee[,1],run$bestmodel$classification)

plot(run)

run2<-clustvarsel(x)
run2
Xs = x[,run2$subset]
run2a = Mclust(Xs, G = 1:5)
summary(run2a)
table(coffee[,1], run2a$classification)

# Smaller wine data
data(wine,package="gclus")
x<-scale(wine[,-c(1)])
run<-vscc(x)
run
table(wine[,1],run$bestmodel$classification)
plot(run)

# Larger wine data
data(wine,package="pgmm")
x<-scale(wine[,-c(1)])
run<-vscc(x)
run
table(wine[,1],run$bestmodel$classification)
plot(run)

# Olive oil data
data(olive,package="pgmm")
??olive

x<-scale(olive[,-c(1,2)])
run<-vscc(x)
run
table(olive[,2],run$bestmodel$classification)
table(olive[,1],run$bestmodel$classification)
??olive

plot(run)