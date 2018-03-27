library(tree)

library(tidyverse)
library(MASS)
library(skimr)
library(GGally)
library(e1071)
library(randomForest)
library(gbm)
library(teigen)
library(skimr)
library(pgmm)


##load data
data("biopsy")
biopsy<-biopsy[,-1]

#check na
any(is.na(biopsy))
biopsy<-na.omit(biopsy)

##switch to numeric 
biopsy[,-10]<-lapply(biopsy[,-10], as.numeric)


##summary
skim(biopsy)

#corr plot
g11 <- ggpairs(data=biopsy, title="biopsy data",
              mapping=ggplot2::aes(colour = class),
              lower=list(combo=wrap("facethist",binwidth=1)))
print(g11)




###classification begin


### tree
set.seed(1)
train = sample (1: nrow(biopsy), nrow(biopsy)*0.75)
tree.biopsy <- tree(class~., data = biopsy, subset=train,method="class")
summary(tree.biopsy)
plot(tree.biopsy)
text(tree.biopsy)
biopsy.test=biopsy[-train,"class"]
biopsy.pred=predict(tree.biopsy,biopsy[-train,],type="class")
tab<-table(biopsy.test,biopsy.pred)
classAgreement(tab)


A1<-c()
B1<-c()
for(i in 1:10){
  set.seed(i)
  train = sample (1: nrow(biopsy), nrow(biopsy)*0.75)
  tree.biopsy <- tree(class~., data = biopsy, subset=train,method="class")
  biopsy.test=biopsy[-train,"class"]
  biopsy.pred=predict(tree.biopsy,biopsy[-train,],type="class")
  tab<-table(biopsy.test,biopsy.pred)
  a<-1-classAgreement(tab)$diag
  b<-classAgreement(tab)$crand
  A1<-c(a,A1)
  B1<-c(b,B1)
}

############################################ end of tree



### Now, bagging and random forests
set.seed(1)
train = sample (1: nrow(biopsy), nrow(biopsy)*0.75)
bag.biopsy=randomForest(class~.,data=biopsy,subset=train,mtry=9,importance=TRUE,type="class")
bag.biopsy
biopsy.test=biopsy[-train,"class"]
biopsy.pred=predict(bag.biopsy,biopsy[-train,],type="class")
tab<-table(biopsy.test,biopsy.pred)
tab
classAgreement(tab)
importance(bag.biopsy)
varImpPlot(bag.biopsy)


## simulation steps
A2<-c()
B2<-c()
for(i in 1:10){
  set.seed(i)
  train = sample (1: nrow(biopsy), nrow(biopsy)*0.75)
  bag.biopsy=randomForest(class~.,data=biopsy,subset=train,mtry=9,importance=TRUE,type="class")
  biopsy.test=biopsy[-train,"class"]
  biopsy.pred=predict(bag.biopsy,biopsy[-train,],type="class")
  tab<-table(biopsy.test,biopsy.pred)
  a<-1-classAgreement(tab)$diag
  b<-classAgreement(tab)$crand
  A2<-c(a,A2)
  B2<-c(b,B2)
}

############################################ end of bagging



set.seed(1)
train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
rf.biopsy=randomForest(class~.,data=biopsy,subset=train,mtry=5,importance=TRUE,type="class")
rf.biopsy
biopsy.pred=predict(rf.biopsy,biopsy[-train,],type="class")
biopsy.test<-biopsy[-train,"class"]
tab<-table(biopsy.test,biopsy.pred)
tab
classAgreement(tab)
importance(rf.biopsy)
varImpPlot(rf.biopsy)


## This is the same but with mtry changed (from 5 to 3)
set.seed(1)
train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
rf.biopsy=randomForest(class~.,data=biopsy,subset=train,mtry=3,importance=TRUE,type="class")
rf.biopsy
biopsy.test<-biopsy[-train,"class"]
biopsy.pred.rf=predict(rf.biopsy,biopsy[-train,],type="class")
tab<-table(biopsy.test,biopsy.pred.rf)

classAgreement(tab)
importance(rf.biopsy)
varImpPlot(rf.biopsy)

##Cross validation for RF, the result is not as good as expected, so using mtry=3, and 500 trees are better
set.seed(123)
train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
biopsy_rf = tune.randomForest(class~., data = biopsy[train,], mtry = 1:9,ntree=100*1:5,tunecontrol = tune.control(sampling = "cross",cross=5))
summary(biopsy_rf)
plot(biopsy_rf)
rf.biopsy<-randomForest(class~.,data=biopsy,subset=train,mtry=1,ntree=500,importance=TRUE,type="class")
rf.biopsy


##sim step with parameter mtry=3, and tree=500.
##simulation step
A3<-c()
B3<-c()
for(i in 1:10){
  set.seed(i)
  train = sample (1: nrow(biopsy), nrow(biopsy)*0.75)
  rf.biopsy=randomForest(class~.,data=biopsy,subset=train,mtry=3,importance=TRUE,type="class")
  biopsy.test=biopsy[-train,"class"]
  biopsy.pred=predict(rf.biopsy,biopsy[-train,],type="class")
  tab<-table(biopsy.test,biopsy.pred)
  a<-1-classAgreement(tab)$diag
  b<-classAgreement(tab)$crand
  A3<-c(a,A3)
  B3<-c(b,B3)
}


############################################ end of random forrest






### Boosting

##binary response, dist is bernoulli, so change factor to numeric,{0,1}
biopsy_1<-biopsy
biopsy_1[,10]<-as.numeric(biopsy_1[,10])
biopsy_1$class[which(biopsy_1$class==1)]<-0
biopsy_1$class[which(biopsy_1$class==2)]<-1

## start boosting
set.seed(1)
train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
biopsy.test<-biopsy[-train,"class"]
boost.biopsy=gbm(class~.,data=biopsy_1[train,],distribution="bernoulli",n.trees=5000,interaction.depth=4)
# We get a plot with summary
summary(boost.biopsy)
# Using type="response" means that class membership probabilities are returned for each observation 
yhat.boost=round(predict(boost.biopsy,newdata=biopsy_1[-train,],n.trees=5000,distribution="bernoulli",type="response"))
# We can do a few different things with these; let's just harden them (for now)
tab<-table(biopsy.test,yhat.boost)
classAgreement(tab)$crand


# Increase lambda to 0.01 (default is 0.001), keep d=4
set.seed(1)
train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
biopsy.test<-biopsy[-train,"class"]
boost.biopsy=gbm(class~.,data=biopsy_1[train,],distribution="bernoulli",n.trees=5000,interaction.depth=4,shrinkage=0.01)
par(mfrow=c(1,2))
summary(boost.biopsy)
gbm.perf(boost.biopsy)##427
yhat.boost=round(predict(boost.biopsy,
                         newdata=biopsy[-train,],
                         n.trees=gbm.perf(boost.biopsy),
                         distribution="bernoulli",
                         type="response"))
tab<-table(biopsy.test,yhat.boost)
tab
1-classAgreement(tab)$diag # 0.035
classAgreement(tab)$crand  # 0.8617


yhat.boost=round(predict(boost.biopsy,newdata=biopsy_1[-train,],n.trees=gbm.perf(boost.biopsy)+300,distribution="bernoulli",type="response"))
tab<-table(biopsy.test,yhat.boost)
tab
1-classAgreement(tab)$diag 
classAgreement(tab)$crand

# Reduce lambda to 0.01 (default is 0.001), put d=1
set.seed(1)
train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
biopsy.test<-biopsy[-train,"class"]
boost.biopsy=gbm(class~.,data=biopsy_1[train,],distribution="bernoulli",n.trees=5000,interaction.depth=1,shrinkage=0.01)
summary(boost.biopsy)
#544 trees
gbm.perf(boost.biopsy) 
# Predict class memebrship proabailities
yhat.boost=round(predict(boost.biopsy,newdata=biopsy_1[-train,],n.trees=gbm.perf(boost.biopsy),distribution="bernoulli",type="response"))
tab<-table(biopsy.test,yhat.boost)
tab
1-classAgreement(tab)$diag 
classAgreement(tab)$crand

yhat.boost=round(predict(boost.biopsy,newdata=biopsy[-train,],n.trees=gbm.perf(boost.biopsy)+300,distribution="multinomial",type="response"))
tab<-table(biopsy.test,yhat.boost)
tab
1-classAgreement(tab)$diag 
classAgreement(tab)$crand

##best one is 544 trees with lambda to be 0.01


A4<-c()
B4<-c()
for(i in 1:10){
  set.seed(i)
  train<-sample(1:nrow(biopsy),nrow(biopsy)*0.75)
  biopsy.test<-biopsy[-train,"class"]
  boost.biopsy=gbm(class~.,data=biopsy_1[train,],distribution="bernoulli",n.trees=5000,interaction.depth=1,shrinkage=0.01)
  yhat.boost=round(predict(boost.biopsy,newdata=biopsy_1[-train,],n.trees=gbm.perf(boost.biopsy),distribution="bernoulli",type="response"))
  tab<-table(biopsy.test,yhat.boost)
  a<-1-classAgreement(tab)$diag
  b<-classAgreement(tab)$crand
  A4<-c(a,A4)
  B4<-c(b,B4)
}

############################################ end of boosting



###teigen


x<-scale(biopsy[,-10])
set.seed(1234)

## model selection
tfaith <- teigen(biopsy[,-10], Gs=1:2,  parallel.cores=TRUE,init="uniform", known=cls)
par(mfrow=c(1,2))
plot(tfaith, what = "contour")
plot(tfaith, what = "uncertainty")
summary(tfaith)


##teigen with specified model
cls <- biopsy[,10]
set.seed(1234)
cls[sample(1:nrow(biopsy),nrow(biopsy)*0.25)] <- NA
tiris <- teigen(biopsy[,-10], parallel.cores=TRUE, models="UUCU", init="uniform", known=cls)
tab<-table(biopsy[,10],tiris$classification)
tab
classAgreement(tab)



A5<-c()
B5<-c()
for(i in 1:10){
  set.seed(i)
  cls <- biopsy[,10]
  cls[sample(1:nrow(biopsy),nrow(biopsy)*0.25)] <- NA
  tiris <- teigen(biopsy[,-10], parallel.cores=TRUE, models="UUCU", init="uniform", known=cls)
  tab<-table(biopsy[,10],tiris$classification)
  a<-1-classAgreement(tab)$diag
  b<-classAgreement(tab)$crand
  A5<-c(a,A5)
  B5<-c(b,B5)
}
########################################## end of teigen



###qda with pca

##pca
x<-biopsy[,-10] 
x<-scale(x) 
cls2<-as.numeric(biopsy[,10])
set.seed(123)
cls2[sample(1:nrow(biopsy),nrow(biopsy)*0.25)] <- 0
pairs(x,col=biopsy[,10])
biopsy_pca <- prcomp(biopsy[,-10], scale = TRUE)
biopsy_pca
summary(biopsy_pca)

pairs(biopsy_pca$x,col=biopsy[,10])

pairs(biopsy_pca$x[,1:2],col=biopsy[,10])

library(MASS)
library(e1071)
# Run qda using PC1-PC2 (which explain around 90% of the variance)
qda_olive<-qda(biopsy_pca$x[,1:2],grouping=biopsy[,10],subset=(cls2!=0))
pred_class<-predict(qda_olive,biopsy_pca$x[(cls2==0),1:2])$class
table(biopsy[(cls2==0),10],pred_class)->tab2
tab2
classAgreement(tab2)


A6<-c()
B6<-c()
for(i in 1:10){
  set.seed(i)
  cls2<-as.numeric(biopsy[,10])
  cls2[sample(1:nrow(biopsy),170)] <- 0
  ##pca
  biopsy_pca <- prcomp(biopsy[,-10], scale = TRUE)
  
  ##qda
  qda_olive<-qda(biopsy_pca$x[,1:2],grouping=biopsy[,10],subset=(cls2!=0))
  pred_class<-predict(qda_olive,biopsy_pca$x[(cls2==0),1:2])$class
  tab<-table(biopsy[(cls2==0),10],pred_class)
  a<-1-classAgreement(tab)$diag
  b<-classAgreement(tab)$crand
  A6<-c(a,A6)
  B6<-c(b,B6)
}
###########################end of qda with pca



df1<-data.frame(cbind(A1,B1))
colnames(df1)<-c("ER","ARI")
df1$class<-rep("tree",10)

df2<-data.frame(cbind(A2,B2))
colnames(df2)<-c("ER","ARI")
df2$class<-rep("bagging",10)


df3<-data.frame(cbind(A3,B3))
colnames(df3)<-c("ER","ARI")
df3$class<-rep("rf",10)

df4<-data.frame(cbind(A4,B4))
colnames(df4)<-c("ER","ARI")
df4$class<-rep("boosting",10)

df5<-data.frame(cbind(A5,B5))
colnames(df5)<-c("ER","ARI")
df5$class<-rep("teigen",10)


df6<-data.frame(cbind(A6,B6))
colnames(df6)<-c("ER","ARI")
df6$class<-rep("qda",10)


df<-bind_rows(df1,df2,df3,df4,df5,df6)
df<-(df%>%arrange(class))

write.csv(df,"plot_data.csv")

df_wide<-bind_cols(df1,df2,df3,df4,df5,df6)
compared_table<-(df_wide%>%select(ARI,ARI1,ARI2,ARI3,ARI4,ARI5)
                 %>%rename(Tree=ARI,
                           bagging=ARI1,
                           RF=ARI2,
                           Boosting=ARI3,
                           Teigen=ARI4,
                           QDA=ARI5))

#write.csv(compared_table,"comp_table.csv")

cp<-ggplot(df,aes(x=class,y=ARI,color=class))
print(cp+geom_boxplot())

p1<-ggplot(df,aes(x=ARI,y=class,color=class))
p2<-p1+geom_line()+geom_point()


t1<-(df%>%
       group_by(class)%>%
       summarise(mean_ER=mean(ER),sd_ER=sd(ER)))

t2<-(df%>%
       group_by(class)%>%
       summarise(mean_ARI=mean(ARI),sd_ARI=sd(ARI)))


##order it by mean ARI, high -->low
table<-(bind_cols(t1,t2)%>%arrange(mean_ER))
table










