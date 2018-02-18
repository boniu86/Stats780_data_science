rm(list=ls())
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 4")
library(MASS)
library(ggplot2)
library("ggthemes")
library("GGally")
library("extracat")
library(hdrcde)
library(KernSmooth)
library("ggplot2")
library("gridExtra")
library("vcd")

#### Iris Data
data(iris)
head(iris)

ggpairs(iris,aes(col=Species, alpha=0.4))

# LDA for iris data
train<-sample(1:150,120)

#x: contains the explanatory variables.
#grouping: a factor specifying the class for each observation.

lda_iris<-lda(x=iris[,-5],grouping=iris[,5],subset=train)
pred_class<-predict(lda_iris,iris[-train,-5])$class
table(iris[-train,5],pred_class)

# QDA for iris data
qda_iris<-qda(x=iris[,-5],grouping=iris[,5],subset=train)
pred_class<-predict(qda_iris,iris[-train,-5])$class
table(iris[-train,5],pred_class)

#### Olive Oil Data
data(olive,package="pgmm")
??olive

str(olive)

ggpairs(olive, aes(alpha=0.4))

olive$Region<-as.factor(olive$Region)
ggpairs(olive[,-2],aes(col=Region, alpha=0.4))

olive$Area<-as.factor(olive$Area)
ggpairs(olive[,-1],aes(col=Area, alpha=0.4))

# MDS for olive
#Multidimensional scaling takes a set of dissimilarities and returns a set of points such that the distances between #the points are approximately equal to the dissimilarities.

d<-dist(olive[,-c(1,2)])
fit <- cmdscale(d,2)
plot(fit,col=olive[,1])

plot(fit,col=olive[,2])

# LDA for olive by area
dim(olive)
train<-sample(1:572,400)
lda_olive<-lda(x=olive[,-c(1,2)],grouping=olive[,1],subset=train)
pred_class<-predict(lda_olive,olive[-train,-c(1,2)])$class
table(olive[-train,1],pred_class)
# QDA for olive by area
qda_olive<-qda(x=olive[,-c(1,2)],grouping=olive[,1],subset=train)
pred_class<-predict(qda_olive,olive[-train,-c(1,2)])$class
table(olive[-train,1],pred_class)
# LDA for olive by region
train<-sample(1:572,400)
lda_olive<-lda(x=olive[,-c(1,2)],grouping=olive[,2],subset=train)
pred_class<-predict(lda_olive,olive[-train,-c(1,2)])$class
table(olive[-train,2],pred_class)
# QDA for olive by region
qda_olive<-qda(x=olive[,-c(1,2)],grouping=olive[,2],subset=train)
pred_class<-predict(qda_olive,olive[-train,-c(1,2)])$class
table(olive[-train,2],pred_class)

####### kNN starts here #######
library(class)

# Bank Data
data(bank,package="gclus")
head(bank)
bank$Status<-factor(bank$Status)

ggpairs(bank, aes(alpha=0.4))
ggpairs(bank,aes(col=Status, alpha=0.4))

test_bnk <- seq(from=1,to=nrow(bank),by=4)
train_bnk <- c(1:nrow(bank))[-test_bnk]

train.set <- bank[train_bnk,]
test.set <- bank[test_bnk,]
labels <- factor(x=bank[train_bnk,1])

# kNN for k = 1
output <- knn(train=train.set[,-1], test=test.set[,-1], cl=labels, k=1, prob=TRUE) 
table(output,test.set[,1])

# kNN for k = 1,3,5,7,9
for(k in c(1,3,5,7,9)){
	cat(paste("There are",k,"nearest neighbours"),"\n")
	output <- knn(train=train.set[,-1], test=test.set[,-1], cl=labels, k=k, prob=TRUE)
	print(output)
	print(table(output,test.set[,1]))
}
	
### Wine data (p=13) from gclus
data(wine,package="gclus")
head(wine)

wine$Class<-factor(wine$Class)
ggpairs(wine,aes(col=Class, alpha=0.4))

s.wine <- scale(wine[,-1])
s.wine <- cbind(wine[,1],s.wine)

train.vals <- sample(c(1:nrow(s.wine)),120)

train.set <- s.wine[train.vals,]
test.set <- s.wine[-train.vals,]
labels <- factor(x=s.wine[train.vals,1])

prop <- 0.8
validation.set <- sample(c(1:nrow(train.set)),size=prop*length(train.vals))
labels.validation.set <- factor(x=train.set[validation.set,1])

for(k in c(1,3,5,7,9,11,13,15)){
	cat(paste("There are",k,"nearest neighbours in the validation set"),"\n")
	output <- knn(train=train.set[validation.set,-1], test=train.set[-validation.set,-1], cl=labels.validation.set, k=k, prob=TRUE)
	print(output)
	class.table <- table(output,train.set[-validation.set,1])
	print(class.table)
	print(sum(diag(class.table))/sum(class.table))
}
	
for(k in c(7,9,11)){
	cat(paste("There are",k,"nearest neighbours"),"\n")
	output <- knn(train=train.set[,-1], test=test.set[,-1], cl=labels, k=k, prob=TRUE)
	print(output)
	class.table <- table(output,test.set[,1])
	print(class.table)
	print(sum(diag(class.table))/sum(class.table))
}
	
# And this is what happens if the data are not scaled...
train.vals <- sample(c(1:nrow(wine)),120)

train.set <- wine[train.vals,]
test.set <- wine[-train.vals,]
labels <- factor(x=wine[train.vals,1])

prop <- 0.8
validation.set <- sample(c(1:nrow(train.set)),size=prop*length(train.vals))
labels.validation.set <- factor(x=train.set[validation.set,1])

for(k in c(1,3,5,7,9,11,13,15)){
	cat(paste("There are",k,"nearest neighbours in the validation set"),"\n")
	output <- knn(train=train.set[validation.set,-1], test=train.set[-validation.set,-1], cl=labels.validation.set, k=k, prob=TRUE)
	print(output)
	class.table <- table(output,train.set[-validation.set,1])
	print(class.table)
	print(sum(diag(class.table))/sum(class.table))
}
	
for(k in c(9)){
	cat(paste("There are",k,"nearest neighbours"),"\n")
	output <- knn(train=train.set[,-1], test=test.set[,-1], cl=labels, k=k, prob=TRUE)
	print(output)
	class.table <- table(output,test.set[,1])
	print(class.table)
	print(sum(diag(class.table))/sum(class.table))
}