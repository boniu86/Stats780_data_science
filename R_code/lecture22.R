## Example using data from ROCR
#install.packages("ROCR")
library(ROCR)
??ROCR
#look-up ROCR simple


# Binary classification examples
data(ROCR.simple)
# Predicted probabilities
head(ROCR.simple$predictions)

# As ususal, we can use 0.5 as a cut-off
cutoff<-0.5
classes<-rep(0,length(ROCR.simple$predictions))
indices<-which(ROCR.simple$predictions>cutoff)
classes[indices]<-1
classes
table(ROCR.simple$labels,classes)

misclassification_rate<-(16+14)/(91+16+14+79)
sensitivity<-79/(79+14)
specificity<-91/(91+16)
1-misclassification_rate
sensitivity
specificity
# ...all around 85%

# But suppose we use a different cut-off for an event, e.g., 0.4:
cutoff<-0.4
classes<-rep(0,length(ROCR.simple$predictions))
indices<-which(ROCR.simple$predictions>cutoff)
classes[indices]<-1
classes
head(ROCR.simple$predictions)
table(ROCR.simple$labels,classes)

misclassification_rate<-(37+14)/(70+37+14+79)
sensitivity<-79/(79+14)
specificity<-70/(70+37)
1-misclassification_rate
sensitivity
specificity
# ...same sensitivity but worse misclassification rate and specificity.

# It would be useful to look at all options for cut-off on a chart...
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
# This is an ROC curve:
plot(perf,ylab="Sensitivity",xlab="1-Specificity")
# This is the tpr (sensitivity), fpr (1-specificity), and corresponding cut-offs:
perf

#around observaation 96
cutoff<-0.501489336
classes<-rep(0,length(ROCR.simple$predictions))
indices<-which(ROCR.simple$predictions>cutoff)
classes[indices]<-1
classes
table(ROCR.simple$labels,classes)

misclassification_rate<-(16+14)/(91+16+14+79)
sensitivity<-79/(79+14)
specificity<-91/(91+16)
1-misclassification_rate
sensitivity
specificity

###############################
library(pgmm)

data(bankruptcy,package="MixGHD")
??bankruptcy
head(bankruptcy)



plot(bankruptcy[,-1],col=bankruptcy[,1]+1)

x<-scale(bankruptcy[,-1])
cls<-bankruptcy[,1]+1
dim(x)
for(i in 1:66){
   if(i%%3==0){cls[i]<-0}
}

bankruptcy_class<-pgmmEM(x,2:2,1:1,cls,relax=TRUE)
cls_ind<-(cls==0) 
tab<-table(bankruptcy[cls_ind,1],bankruptcy_class$map[cls_ind])
tab

#zhat: A matrix giving the raw values upon which map is based.
bankruptcy_class$zhat[cls_ind,1]

?performance
cls2<-(bankruptcy[cls_ind,1]+1)%%2
pred<-prediction(bankruptcy_class$zhat[cls_ind,1],cls2)
perf <- performance(pred, "tpr", "fpr")
plot(perf,ylab="Sensitivity",xlab="1-Specificity")
perf
##########
data(banknote,package="mclust")
head(banknote)
x<-scale(banknote[,-1])
cls<-c(rep(1,100),rep(2,100))
for(i in 1:200){
   if(i%%4==0){cls[i]<-0}
}

banknote_class<-pgmmEM(x,2:2,1:3,cls,relax=TRUE)
cls_ind<-(cls==0) 
tab<-table(banknote[cls_ind,1],banknote_class$map[cls_ind])
tab

cls2<-(bankruptcy[cls_ind,1]+1)%%2
pred<-prediction(banknote_class$zhat[cls_ind,1],banknote[cls_ind,1])
perf <- performance(pred, "tpr", "fpr")
plot(perf,ylab="Sensitivity",xlab="1-Specificity")
