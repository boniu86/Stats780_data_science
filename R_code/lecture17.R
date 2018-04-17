### Mixture DA
library(mclust)

## Olive oil 
data("olive",package="pgmm") 
x<-olive[,-c(1,2)] 
x<-scale(x) 

olive_delete<-rep(0,143)
k<-1
for(i in 1:dim(olive)[1]){
   if(i%%4==0){olive_delete[k]<-i; k<-k+1}
}

oliveMclustDA <- MclustDA(x[-olive_delete,], olive[-olive_delete,2])
summary(oliveMclustDA, parameters = TRUE)
summary(oliveMclustDA, newdata = x[olive_delete,], newclass = olive[olive_delete,2])

## Banknote data
data(banknote,package="mclust")
x<-as.matrix(banknote[,-1])
x<-banknote[,-1] 

cls3<-c(rep(1,100),rep(2,100))
bank_delete<-rep(0,50)
k<-1
for(i in 1:dim(banknote)[1]){
   if(i%%4==0){bank_delete[k]<-i; k<-k+1}
}
bankMclustDA <- MclustDA(x[-bank_delete,], cls3[-bank_delete])
summary(bankMclustDA, parameters = TRUE)

summary(bankMclustDA, newdata = x[bank_delete,], newclass = cls3[bank_delete])

### Model-based classification using teigen
library(teigen)
?teigen

## Iris data
cls <- iris[,5]
set.seed(1234)
cls[sample(1:nrow(iris),50)] <- NA
tiris <- teigen(iris[,-5], parallel.cores=2, models="CUUU", init="uniform", known=cls)
table(iris[,5],tiris$classification)



