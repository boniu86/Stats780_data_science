### Mixture DA
library(mclust)

library(tree)
library(e1071)

library(MASS)
library(tidyverse)


data("biopsy")
user<-biopsy[,-1]
user<-na.omit(user)

user[,-10]<-lapply(user[,-10], as.numeric)


##  MDA
x<-user[,-10]
x<-scale(x)
B<-c()
C<-c()
D<-c()
Tab<-union()
for(i in 1:10){
  set.seed(i)
  user_delete<-sample (1: nrow(user), nrow(user)/4)
  userMclustDA <- MclustDA(x[-user_delete,], user[-user_delete,10])
  summary(userMclustDA, parameters = TRUE)
  a<-summary(userMclustDA, newdata = x[user_delete,], newclass = user[user_delete,10])
  
  b<-classAgreement(a$tab.newdata)$diag
  c<-classAgreement(a$tab.newdata)$rand
  d<-classAgreement(a$tab.newdata)$crand
  B<-c(b,B)
  C<-c(c,C)
  D<-c(d,D)
  
}

set.seed(123)
user_delete<-sample (1: nrow(user), nrow(user)/4)
userMclustDA <- MclustDA(x[-user_delete,], user[-user_delete,10])
summary(userMclustDA, parameters = TRUE)
summary(userMclustDA, newdata = x[user_delete,], newclass = user[user_delete,10])


df1<-data.frame(cbind(B,C,D))
colnames(df1)<-c("diag","rand","crand")
df1$class<-rep("MDA",10)



##Tree
BB<-c()
CC<-c()
DD<-c()
for(i in 1:10){
  set.seed(i)
  train = sample (1: nrow(user), nrow(user)*0.75)
  user$class<-factor(user$class)
  tree.user <- tree(class~., data =user, subset=train,method="class")
  user.test=user[-train,"class"]
  user.pred=predict(tree.user,user[-train,],type="class")
  tab<-table(user.test,user.pred)
  bb<-classAgreement(tab)$diag
  cc<-classAgreement(tab)$rand
  dd<-classAgreement(tab)$crand
  BB<-c(bb,BB)
  CC<-c(cc,CC)
  DD<-c(dd,DD)
}

set.seed(123)
train = sample (1: nrow(user), nrow(user)*0.75)
user$class<-factor(user$class)
tree.user <- tree(class~., data =user, subset=train,method="class")
user.test=user[-train,"class"]
user.pred=predict(tree.user,user[-train,],type="class")
tab<-table(user.test,user.pred)

df2<-data.frame(cbind(BB,CC,DD))
colnames(df2)<-c("diag","rand","crand")
df2$class<-rep("tree",10)

df<-union(df1,df2)
df<-(df%>%arrange(class))

t1<-(df%>%
    group_by(class)%>%
    summarise(mean_crand=mean(crand),sd_crand=sd(crand)))

t2<-(df%>%
    group_by(class)%>%
    summarise(mean_rand=mean(rand),sd_rand=sd(rand)))

t3<-(df%>%
     group_by(class)%>%
     summarise(mean_diag=mean(diag),sd_diag=sd(diag)))

table<-bind_cols(t1,t2,t3)
table

dd<-read_csv("df.csv")
ddf<-dd[,-1]

p1<-ggplot(ddf,aes(x=class,y=crand))+geom_boxplot()


p2<-ggplot(ddf,aes(x=class,y=rand))+geom_boxplot()

p3<-ggplot(ddf,aes(x=class,y=diag))+geom_boxplot()

library(gridExtra)

grid.arrange(p1,p2,p3,ncol=3)
