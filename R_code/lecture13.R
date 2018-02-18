# Iris data
library(mclust)
mod1 = Mclust(iris[,-5])
summary(mod1)

plot(mod1,  what = c("BIC", "classification"))

mod2 = Mclust(iris[,-5])
summary(mod2)
plot(mod2,  what = c("classification"))
table(iris[,5],map(mod2$z))

mod2a = Mclust(iris[,-5],3:3)
summary(mod2a)
plot(mod2a,  what = c("classification"))
table(iris[,5],map(mod2a$z))

# Coffee data
data(coffee,package="pgmm")
mod3 = Mclust(coffee[,-c(1,2)])
summary(mod3)

plot(mod3,  what = c("classification"))

table(coffee[,1],map(mod3$z))->tab3
tab3

library(e1071)
classAgreement(tab3)

plot(coffee[,c(9,10)],col=map(mod3$z),pch=coffee[,1])

# This one, i.e., the wine data, might take a little bit of time... 
data(wine,package="gclus")
mod4 = Mclust(wine[,-1])
summary(mod4)

plot(mod4,  what = c("classification"))


table(wine[,1],map(mod4$z))->tab4
tab4

classAgreement(tab4)
