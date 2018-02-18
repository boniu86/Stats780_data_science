rm(list=ls())
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 5")

library(randomForest)
library(MASS)
library(tree)
library(gbm)
library(e1071)

par(mfrow=c(1,1))
# Plot for M/3 explanation
n<-10:1000
plot(n,((n-1)/n)^n,typ="l")
dev.print(device=postscript, "M3.eps", onefile=FALSE, horizontal=FALSE)

### Boston example, based on example from James et al.(2013)
library(randomForest)
library(MASS)
library(tree)

# First, a tree
?Boston

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
#median value of owner-occupied homes in \$1000s
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)


plot(tree.boston)
text(tree.boston,pretty=0)

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

#######################################################
####### Now, bagging and random forests################

#bagging
set.seed(1)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
plot(yhat.bag,boston.test)
abline(0,1)

importance(bag.boston)

varImpPlot(bag.boston)

###
##random forest
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=4,importance=TRUE)
rf.boston
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)

varImpPlot(rf.boston)

###
rf.boston2=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE,ntree=500)
rf.boston2
yhat.rf = predict(rf.boston2,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)

varImpPlot(rf.boston)
##############################
# Boosting -- 

set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
boston.test=Boston[-train,"medv"]
mean((yhat.boost-boston.test)^2)

### Hitters data
data(Hitters,package="ISLR")
head(Hitters)
Hitters<-na.omit(Hitters)
Hitters$logSalary<-log(Hitters$Salary)
names(Hitters)
str(Hitters)

Hitters<-Hitters[,-19]
names(Hitters)
set.seed(1)

train = sample (1: nrow(Hitters), nrow(Hitters)/2)
tree.hitters <- tree(logSalary ~ ., data = Hitters, subset=train)
summary(tree.hitters)
par(mfrow=c(1,1))
plot(tree.hitters)
text(tree.hitters)

yhat=predict(tree.hitters,newdata=Hitters[-train,])
hitters.test=Hitters[-train,"logSalary"]
plot(yhat,hitters.test)
abline(0,1)

mean((yhat-hitters.test)^2)
# Now, bagging and random forests
set.seed(1)
bag.hitters=randomForest(logSalary~.,data=Hitters,subset=train,mtry=19,importance=TRUE)
bag.hitters

yhat.bag = predict(bag.hitters,newdata=Hitters[-train,])
mean((yhat.bag-hitters.test)^2)

importance(bag.hitters)

varImpPlot(bag.hitters)

plot(yhat.bag,hitters.test)
abline(0,1)

set.seed(1)
rf.hitters=randomForest(logSalary~.,data=Hitters,subset=train,mtry=5,importance=TRUE)
rf.hitters

yhat.rf = predict(rf.hitters,newdata=Hitters[-train,])
mean((yhat.rf-hitters.test)^2)

importance(rf.hitters)

varImpPlot(rf.hitters)

#########################
#A figure
M<-(1:20)*50
error<-M
for(m in M){
	bag.hitters=randomForest(logSalary~.,data=Hitters,subset=train,mtry=19,importance=TRUE,ntree=m)
	yhat.bag = predict(bag.hitters,newdata=Hitters[-train,])
	error[m/50]<-mean((yhat.bag-hitters.test)^2)
}
bag<-error
for(m in M){
	bag.hitters=randomForest(logSalary~.,data=Hitters,subset=train,mtry=9,importance=TRUE,ntree=m)
	yhat.bag = predict(bag.hitters,newdata=Hitters[-train,])
	error[m/50]<-mean((yhat.bag-hitters.test)^2)
}
rf4<-error
for(m in M){
	bag.hitters=randomForest(logSalary~.,data=Hitters,subset=train,mtry=5,importance=TRUE,ntree=m)
	yhat.bag = predict(bag.hitters,newdata=Hitters[-train,])
	error[m/50]<-mean((yhat.bag-hitters.test)^2)
}
rf6<-error
plot(M,bag,typ="l",col=2,xlim=c(0,1000),ylab="test error",ylim=c(0.145,0.2))
lines(M,rf4,typ="l",col=1)
lines(M,rf6,typ="l",col=4)
legend(1,0.165,c("m=19 (bagging)","m=9","m=5"),col=c(2,1,4),pch=c(2,3,4))

##################################
# Boosting for Hitters data 
set.seed(1)
boost.hitters=gbm(logSalary~.,data=Hitters[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.hitters)

yhat.boost=predict(boost.hitters,newdata=Hitters[-train,],n.trees=5000)
hitters.test=Hitters[-train,"logSalary"]
mean((yhat.boost-hitters.test)^2)

### Wine data
data(wine,package="gclus")
names(wine)
### wine data
set.seed(1)
train = sample (1: nrow(wine), nrow(wine)/2)
wine$Class<-factor(wine$Class)
tree.wine <- tree(Class~., data = wine, subset=train,method="class")
summary(tree.wine)
plot(tree.wine)
text(tree.wine)
wine.test=wine[-train,"Class"]
wine.pred=predict(tree.wine,wine[-train,],type="class")
tab<-table(wine.test,wine.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
# Now, bagging and random forests
set.seed(1)
bag.wine=randomForest(Class~.,data=wine,subset=train,mtry=13,importance=TRUE,type="class")
bag.wine
wine.pred=predict(bag.wine,wine[-train,],type="class")
tab<-table(wine.test,wine.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand
importance(bag.wine)
varImpPlot(bag.wine)

set.seed(1)
rf.wine=randomForest(Class~.,data=wine,subset=train,mtry=3,importance=TRUE,type="class")
rf.wine
wine.pred=predict(rf.wine,wine[-train,],type="class")
tab<-table(wine.test,wine.pred)
tab
1-classAgreement(tab)$diag
classAgreement(tab)$crand

importance(bag.wine)
varImpPlot(bag.wine)

set.seed(1)
# This is the same but with mtry changed (from 3 to 2)
rf.wine=randomForest(Class~.,data=wine,subset=train,mtry=2,importance=TRUE,type="class")
rf.wine
wine.pred.rf=predict(rf.wine,wine[-train,],type="class")
tab<-table(wine.test,wine.pred.rf)
tab
1-classAgreement(tab)$diag # I get 0.034 here, which is very good.
classAgreement(tab)$crand

importance(rf.wine)
varImpPlot(rf.wine)

### Boosting
set.seed(1)
boost.wine=gbm(Class~.,data=wine[train,],distribution="multinomial",n.trees=5000,interaction.depth=4)
# We get a plot with summary
summary(boost.wine)
# Using type="response" means that class membership probabilities are returned for each observation 
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=5000,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
class.pred<-rep(0,89)
for(i in 1:89){
	which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag # I get 0.056
classAgreement(tab)$crand


# Increase lambda to 0.01 (default is 0.001), keep d=4
set.seed(1)
boost.wine=gbm(Class~.,data=wine[train,],distribution="multinomial",n.trees=5000,interaction.depth=4,shrinkage=0.01)
par(mfrow=c(1,2))
summary(boost.wine)
# The following is a suggestion about how many trees to use for the test set
# (I get 928 with a warning that it may be an underestimate) as well as a nice plot
# We did *not* do this in class, but it is important (unlike random forests, "overfitting" is a concern here)
gbm.perf(boost.wine) 
# Using type="response" means that class membership probabilities are returned for each observation 
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=928,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
for(i in 1:89){
	which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ....this is very good (as good as the random forest from above)
classAgreement(tab)$crand
# What if 928 was an underestimate... 
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=1200,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
for(i in 1:89){
	which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.034 ...no change
classAgreement(tab)$crand

# Reduce lambda to 0.01 (default is 0.001), put d=1
set.seed(1)
boost.wine=gbm(Class~.,data=wine[train,],distribution="multinomial",n.trees=5000,interaction.depth=1,shrinkage=0.01)
summary(boost.wine)
# This is a suggestion about how many trees to use for the test set (I get 1098 with a warning)
gbm.perf(boost.wine) 
# Predict class memebrship proabailities
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=1098,distribution="multinomial",type="response")
# We can do a few different things with these; let's just harden them (for now)
for(i in 1:89){
	which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag # 0.056 ...not as good
classAgreement(tab)$crand
# Again, suppose 1098 is an underestimate
yhat.boost=predict(boost.wine,newdata=wine[-train,],n.trees=1250,distribution="multinomial",type="response")
for(i in 1:89){
	which(yhat.boost[i,,1]==max(yhat.boost[i,,1]))->class.pred[i]
}
tab<-table(wine.test,class.pred)
tab
1-classAgreement(tab)$diag 
classAgreement(tab)$crand
