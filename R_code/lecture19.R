## Rock data -- example form plot.ppr help
rm(list=ls())
data(rock)
rock
?rock

pairs(rock)
rock$area1 <- rock$area/10000
rock$peri1 <- rock$peri/10000
?ppr

rock.ppr <- ppr(log(perm) ~ area1 + peri1 + shape, data = rock, nterms = 2, max.terms = 5)
summary(rock.ppr)

rock.ppr$alpha
# Some plots
plot(rock.ppr$gofn,type="b")

par(mfrow=c(1,2))
plot(rock.ppr, main="ppr(log(perm)~ ., nterms=2, max.terms=5)")

par(mfrow=c(1,3))
plot(update(rock.ppr, nterms=3), main = "update(..., nterms = 3)")

rock.ppr <- ppr(log(perm) ~ area1 + peri1 + shape, data = rock, nterms = 2, max.terms = 5)

par(mfrow=c(3,2))
plot(rock.ppr, main="ppr(log(perm)~ ., nterms=2, max.terms=5)")
plot(update(rock.ppr, bass=5), main = "update(..., bass = 5)")
plot(update(rock.ppr, sm.method="gcv", gcvpen=2), main = "update(..., sm.method=\"gcv\",gcvpen=2)")

summary(rock.ppr)
# Playing around with settings
rock.ppr <- ppr(log(perm) ~ area1 + peri1 + shape, data = rock, nterms = 2, max.terms = 5)
summary(rock.ppr)

rock.ppr2 <- ppr(log(perm) ~ area1 + peri1 + shape, data = rock, nterms = 2, max.terms = 5, bass=5)
summary(rock.ppr2)

rock.ppr3 <- ppr(log(perm) ~ area1 + peri1 + shape, data = rock, nterms = 2, max.terms = 5, sm.method="gcv", gcvpen=2)
summary(rock.ppr3)

## orings example
library(faraway)
data(orings)
??orings

plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim = c(0,1), xlab="Temperature", ylab="Probability of damage")

orings.ppr <- ppr(damage ~ temp, data = orings, nterms = 1, max.terms = 5)
plot(orings.ppr, main="ppr(damage~temp, nterms=1, max.terms=5)")

#fix graph
head(orings)
new<-orings[1,]
new[1]<-31
new[2]<-0 # This does not matter
predict(orings.ppr,new)
plot(orings$temp,predict(orings.ppr),type="b")
orings1<-orings

#we have 23 observations initially. Observation 24 will correspond to 31 degrees 
orings1[24,]<-new
pred<-predict(orings.ppr)
pred[24]<-predict(orings.ppr,new)
plot(orings1$temp,pred,type="p") 
