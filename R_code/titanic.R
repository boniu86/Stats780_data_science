rm(list=ls())
library(arules) 
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 1")
source("std_lift.R")
load("titanic.raw.rdata")
head(titanic.raw)

str(titanic.raw)
x<-titanic.raw

params<-list(support=0.005,confidence=0.8,minlen=2,maxlen=6)
fit1<-apriori(x,parameter=params)
inspect(fit1, by="lift") 

fit2<-fit1
quality(fit2)<-std_lift(fit2,x)
inspect(sort(fit2, by = "slift"))

# Suppose we are interested in rules with consequent Survived (yes or no)
app<-list(rhs=c("Survived=No","Survived=Yes"),default="lhs")
fit3<-apriori(x,parameter=params,appearance=app)
quality(fit3)<-std_lift(fit3,x)
inspect(sort(fit3, by = "lift"))

inspect(sort(fit3, by = "slift"))

# What about children?
app <- list(default="none", rhs=c("Survived=Yes"),lhs=c("Class=1st", "Class=2nd", "Class=3rd", "Age=Child", "Age=Adult"))
fit4 <- apriori(x, parameter=params, control = list(verbose=FALSE), appearance=app)
quality(fit4)<-std_lift(fit4,x)
inspect(sort(fit4, by = "slift"))

# Lower the support threshold
params$support<-0.001
fit5 <- apriori(x, parameter=params, control = list(verbose=FALSE), appearance=app)
quality(fit5)<-std_lift(fit5,x)
inspect(sort(fit5, by = "slift"))

# Also, lower the confidence threshold
params$confidence<-0.3
fit6 <- apriori(x, parameter=params, control = list(verbose=FALSE), appearance=app)
quality(fit6)<-std_lift(fit6,x)
inspect(sort(fit6, by = "slift"))
