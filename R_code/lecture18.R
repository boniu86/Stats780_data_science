rm(list=ls())

## Hepathlon data
data("heptathlon", package = "HSAUR")
??heptathlon

head(heptathlon)

# Make it so that bigger numbers are better for every event:
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m
head(heptathlon[,-8])

#PCA based on scaled data
heptathlon_pca <- prcomp(heptathlon[, -8], scale = TRUE)
heptathlon_pca
summary(heptathlon_pca)

#biplot(heptathlon_pca)
head(heptathlon_pca$x)

pairs(heptathlon)

plot(heptathlon_pca$x[,1],heptathlon[, 8])

reg1<-lm(heptathlon[, 8]~heptathlon_pca$x[,1])
summary(reg1)

## Crabs data
library(pgmm)
library(e1071)
data(crabs,package="MASS")
pairs(crabs[,-c(1:3)],col=c(rep(1,50),rep(2,50),rep(3,50),rep(4,50)))

crabs_pca <- prcomp(crabs[,-c(1:3)], scale = TRUE)
crabs_pca
summary(crabs_pca)

pairs(crabs_pca$x,col=c(rep(1,50),rep(2,50),rep(3,50),rep(4,50)))
# Run pgmm on the crabs data
pgmm_crabs<-pgmmEM(crabs[,-c(1:3)],2:4,2:3,relax=TRUE)
table(c(rep(1,50),rep(2,50),rep(3,50),rep(4,50)),pgmm_crabs$map)->tab
tab
classAgreement(tab)
# Run k-means using PC1 (which explains 98.25% of the variance)
kmeans_pca_crabs<-kmeans(crabs_pca$x[,1],4)
table(c(rep(1,50),rep(2,50),rep(3,50),rep(4,50)), kmeans_pca_crabs $cluster)->tab
tab
classAgreement(tab)


## Olive oil data
data(olive, package="pgmm")
x<-olive[,-c(1,2)] 
x<-scale(x) 
cls2<-olive[,2]
olive_delete<-rep(0,143)
k<-1
for(i in 1:dim(olive)[1]){
   if(i%%4==0){cls2[i]<-0; olive_delete[k]<-i; k<-k+1}
}

pairs(x,col=olive[,2])

olive_pca <- prcomp(olive[,-c(1:2)], scale = TRUE)
olive_pca
summary(olive_pca)

pairs(olive_pca$x,col=olive[,2])

pairs(olive_pca$x[,1:4],col=olive[,2])

library(MASS)
library(e1071)
# Run qda using PC1-PC4 (which explain > 90% of the variance)
qda_olive<-qda(olive_pca$x[,1:4],grouping=olive[,2],subset=(cls2!=0))
pred_class<-predict(qda_olive,olive_pca$x[(cls2==0),1:4])$class
table(olive[(cls2==0),2],pred_class)->tab2
tab2
classAgreement(tab2)$crand

# Run pgmm for model-based classification
olive_class2<-pgmmEM(x,9:9,1:6,cls2,relax=TRUE)
cls_ind2<-(cls2==0) 
tab<-table(olive[cls_ind2,2],olive_class2$map[cls_ind2])
tab
classAgreement(tab)

## Finally, it can be interesting to use a table, etc., to compare clustering results
library(cluster)
data(coffee, package="pgmm")
x<-scale(coffee[,-c(1,2)])
set.seed(1)
coffee_kmeans<-kmeans(x,3)
table(coffee[,1],coffee_kmeans$cluster)

set.seed(1)
coffee_kmedoids<-pam(x,3)
table(coffee[,1],coffee_kmedoids$cluster)

table(coffee_kmeans$cluster,coffee_kmedoids$cluster) # Disagree on 4 points...
# ...would be interesting to further consider these points

coffee_kmeans<-kmeans(x,2)
table(coffee[,1],coffee_kmeans$cluster)
set.seed(1)

coffee_kmedoids<-pam(x,2)
table(coffee[,1],coffee_kmedoids$cluster)

table(coffee_kmeans$cluster,coffee_kmedoids$cluster)