library(e1071)
library(pgmm)



### wine
data(wine,package="pgmm")
x<-wine[,-1]
x<-scale(x)
# k-means starts
wine_pgmmEM = pgmmEM(x, rG=1:5,rq=1:5,relax=TRUE)
table(wine[,1],wine_pgmmEM$map) 
summary(wine_pgmmEM)
# k-means starts 2
wine_pgmmEM2 = pgmmEM(x, rG=1:5,rq=1:8,relax=TRUE)
table(wine[,1],wine_pgmmEM2$map) 
summary(wine_pgmmEM2)
# use 5 random values for starting values -- this will be slow
wine_pgmmEM3 = pgmmEM(x, rG=1:5,rq=1:5,relax=TRUE,zstart=1,loop=5)
table(wine[,1],wine_pgmmEM3$map) 

### olive oil -- these are classification examples
data("olive",package="pgmm") 
x<-olive[,-c(1,2)] 
x<-scale(x) 
# 3 regions

#every 4th observation will be labelled as "0", i.e. will have its cluster membership label removed. These observations will be our test dataset.
cls1<-olive[,1]
for(i in 1:dim(olive)[1]){
  if(i%%4==0){cls1[i]<-0}
}
olive_class<-pgmmEM(x,3:3,1:5,cls1,relax=TRUE)
cls_ind1<-(cls1==0) 
tab<-table(olive[cls_ind1,1],olive_class$map[cls_ind1])
tab
classAgreement(tab)
# 9 areas
cls2<-olive[,2]
for(i in 1:dim(olive)[1]){
  if(i%%4==0){cls2[i]<-0}
}
olive_class2<-pgmmEM(x,9:9,1:5,cls2,relax=TRUE)
cls_ind2<-(cls2==0) 
tab2<-table(olive[cls_ind2,2],olive_class2$map[cls_ind2])
tab2
classAgreement(tab2)

### iris
data(iris)
head(iris)
x<-iris[,-5]
x<-scale(x)
iris_pgmmEM = pgmmEM(x, rG=1:5,rq=1:2)
tab_iris<-table(iris[,5],iris_pgmmEM$map) 
tab_iris
tab_iris2<-tab_iris[,c(3,2,1)]
tab_iris
tab_iris2
classAgreement(tab_iris)
classAgreement(tab_iris2)

### coffee
data(coffee,package="pgmm")
x<-coffee[,-c(1,2)]
x<-scale(x)
# k-means starts
coffee_pgmmEM = pgmmEM(x, rG=1:5,rq=1:5)
table(coffee[,1],coffee_pgmmEM$map) 

# use random values for starting values
coffee_pgmmEM2 = pgmmEM(x, rG=1:5,rq=1:5,relax=TRUE,zstart=1,loop=5)
table(coffee[,1],coffee_pgmmEM2$map) 

 
