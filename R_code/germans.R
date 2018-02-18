rm(list=ls())
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 1")
#install.packages("arules")
library(arules) 
source("std_lift.R")
x<-read.transactions("germans_all.dat",format="single",cols=c(1,2))
x

#??arules
params<-list(support=0.2,confidence=0.8,minlen=2,maxlen=6)
fit<-apriori(x,parameter=params) 
qual<-quality(fit)
inspect(sort(fit, by = "lift"))

quality(fit)<-std_lift(fit,x)
inspect(sort(fit, by = "slift"))


