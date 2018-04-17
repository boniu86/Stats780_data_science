rm(list=ls())
library(nnet)
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 9")

### Using nnet for a "vanilla" neural net, i.e., a single-layer perceptron

## Iris data -- we played with size and decay in nnet
head(iris)

x<-scale(iris[,-5])
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
cls<-class.ind(iris[,5])

?nnet
nn_iris<-nnet(x[train,], cls[train,], size=15, decay=2, softmax=TRUE)
nn_pred<-predict(nn_iris, x[-train,], type="class")
table(iris[-train,5],nn_pred)

#change decay (2) and size (15) 

## Seeds data from UCI repository (https://archive.ics.uci.edu/ml/datasets/seeds)
## We played with size (4), decay(0), and maxit (500) in nnet
seeds<-read.csv("seeds.csv",header=TRUE)
head(seeds)
dim(seeds)
seeds$Class

seeds[,1:7]<-scale(seeds[,1:7])
seedstrain<- sample(1:210,160)
seedstest <- setdiff(1:210,seedstrain)
cls <- class.ind(seeds$Class)

nn_seeds = nnet(seeds[seedstrain,-8], cls[seedstrain,], size=4, softmax=TRUE,maxit=500,decay=0)
#predict(nn_seeds, seeds[seedstrain,-8], type="class")
table(predict(nn_seeds, seeds[-seedstrain,-8], type="class"),seeds[-seedstrain,]$Class)

## Wine data; again, we played with arguments to nnet
data(wine,package="pgmm")
x<-scale(wine[,-1])
winetrain<- sample(1:178,130)
cls<-class.ind(wine[,1])

## Started with size (15), decay(1), and maxit (500) in nnet

nn_wine<-nnet(x[winetrain,], cls[winetrain,], size=15, decay=0, softmax=TRUE, maxit=500)
nn_pred<-predict(nn_wine, x[-winetrain,], type="class")
table(wine[-winetrain,1],nn_pred)

## Olive oil data; again we played with arguments to nnet
data(olive,package="pgmm")
head(olive)
olive$Area

x<-scale(olive[,-c(1,2)])
olivetrain<- sample(1:dim(olive)[1],450)

cls<-class.ind(olive[,2])
cls
colSums(cls)

colSums(cls[olivetrain,])

#started with size(20), decay(1), maxit(500)
nn_olive<-nnet(x[olivetrain,], cls[olivetrain,], size=20, decay=1, softmax=TRUE, maxit=500)
nn_pred<-predict(nn_olive, x[-olivetrain,], type="class")
table(olive[-olivetrain,2],nn_pred)

## Boston housing data (regression, not classification); again, we played with settings
data(Boston,package="MASS")
?Boston

head(Boston)
#interested in median house value

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
x<-scale(Boston)

#started with size(10), decay(1), maxit(1000)
nn_boston<-nnet(x[train,-14], x[train,14], size=10, decay=1, softmax=FALSE, maxit=1000,linout=TRUE)
nn_pred<-predict(nn_boston, x[-train,],type="raw")

plot(x[-train,14],nn_pred)

