## Note: all data are scaled in these examples (it seems the tune functions
## do not do this automatically)

rm(list=ls())
library(e1071)
library(nnet)
library(class)
library(randomForest)

## CV for kNN on (scaled) iris data
iris[,-5]<-scale(iris[,-5])
set.seed(1)
train<-sample(1:150,120)
x <- iris[train,-5]
y <- iris[train,5]

?tune.knn
iris_cv <- tune.knn(x, y, k = 1:10, tunecontrol = tune.control(sampling = "cross",cross=5))
summary(iris_cv)
plot(iris_cv)

iris_knn<-knn(train=x, test=iris[-train,-5], cl=y, k=1, prob=TRUE)
table(iris_knn,iris[-train,5])


## CV for neural network on (scaled) iris data: varying size
# size : the number of units in the hidden layer
 
iris_nnet1 = tune.nnet(Species~., data = iris[train,], size = 1:30,tunecontrol = tune.control(sampling = "cross",cross=5))
summary(iris_nnet1)
plot(iris_nnet1)

cls<-class.ind(iris[,5])
x<-iris[,-5]
nn_iris<-nnet(x[train,], cls[train,], size=10, decay=0, softmax=TRUE)
nn_pred<-predict(nn_iris, x[-train,], type="class")
table(iris[-train,5],nn_pred)
#demonstrate for decay=2 and size=10


## CV for neural network on (scaled) iris data: varying size and decay
iris_nnet2 = tune.nnet(Species~., data = iris[train,], size = 1:20,decay=0:3,tunecontrol = tune.control(sampling = "cross",cross=5))
summary(iris_nnet2)
plot(iris_nnet2)

nn_iris2<-nnet(x[train,], cls[train,], size=5, decay=0, softmax=TRUE)
nn_pred<-predict(nn_iris2, x[-train,], type="class")
table(iris[-train,5],nn_pred)


## Bootstrap sampling approach for neural network on (scaled) iris data: varying size
iris_nnet3 = tune.nnet(Species~., data = iris[train,], size = 1:20,decay=0:3,tunecontrol = tune.control(sampling = "boot",nboot=25))
summary(iris_nnet3)
plot(iris_nnet3)

nn_iris3<-nnet(x[train,], cls[train,], size=2, decay=0, softmax=TRUE)
nn_pred<-predict(nn_iris3, x[-train,], type="class")
table(iris[-train,5],nn_pred)

## CV for random forest on (scaled) iris data: varying mtry

# mtry: Number of variables randomly sampled as candidates at each split. 
iris_rf = tune.randomForest(Species~., data = iris[train,], mtry = 1:4,tunecontrol = tune.control(sampling = "cross",cross=5))
summary(iris_rf)
plot(iris_rf)

rf.iris<-randomForest(Species~.,data=iris,subset=train,mtry=1,importance=TRUE,type="class")
rf.iris

iris.pred=predict(rf.iris,iris[-train,],type="class")
table(iris[-train,5],iris.pred)

## CV for random forest on (scaled) wine data: varying mtry and ntree
# ntree: Number of trees to grow
data(wine,package="gclus")
set.seed(1)
train = sample (1: nrow(wine), 120)
wine$Class<-factor(wine$Class)
wine[,2:13]<-scale(wine[,2:13])

wine_rf = tune.randomForest(Class~., data = wine[train,], mtry = 1:13,ntree=100*1:5,tunecontrol = tune.control(sampling = "cross",cross=5))
summary(wine_rf)
plot(wine_rf)

rf.wine<-randomForest(Class~.,data=wine,subset=train,mtry=1,ntree=100,importance=TRUE,type="class")
rf.wine

wine.pred=predict(rf.wine,wine[-train,],type="class")
table(wine[-train,1],wine.pred)