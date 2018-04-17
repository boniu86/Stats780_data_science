### Mixtures of multivariate t-distributions (teigen)
library(teigen)
?teigen
## Old Faithful
tfaith <- teigen(faithful, models="UUUU", Gs=1:3, init="hard")
plot(tfaith, what = "uncertainty")

plot(tfaith, what = "contour")

summary(tfaith)
tfaith$classification

## Smaller wine data (p=13)
data(wine,package="gclus")
x<-scale(wine[,-1])

tfaith <- teigen(x, models="UUUU", Gs=1:3, init="hard")
plot(tfaith, what = "contour")

summary(tfaith)
table(wine[,1],tfaith$classification)

## Now use all teigen models (take away models="UUUU")
tfaith <- teigen(x, Gs=1:3, init="hard",parallel.cores=TRUE,)
plot(tfaith, what = "contour")
summary(tfaith)
table(wine[,1],tfaith$classification)


## Larger wine data (p=28); all models, in parallel
data(wine,package="pgmm")
x<-scale(wine[,-1])

tfaith <- teigen(x, Gs=1:3, init="hard",parallel.cores=TRUE)
plot(tfaith, what = "contour")

summary(tfaith)
table(wine[,1],tfaith$classification)

### Mixtures of generalized hyperbolic distributions
library(MixGHD)

## Banknote data
data(banknote)
model=MGHD(banknote[,2:7],G=2,max.iter=20)
table(banknote[,1],model@map)

## Crabs data 
data(crabs)
x<-scale(crabs[,-c(1,2,3)])
model=MGHD(x,G=1:4,max.iter=50)
table(c(rep(1,50),rep(2,50),rep(3,50),rep(4,50)),model@map)

pairs(crabs[,-c(1,2,3)],col=model@map)
pairs(crabs[,-c(1,2,3,7,8)],col=model@map)

## Larger wine data (p=27)
x<-scale(wine[,-c(1)])
model=MGHD(x,G=3,max.iter=50)
table(wine[,1],model@map)