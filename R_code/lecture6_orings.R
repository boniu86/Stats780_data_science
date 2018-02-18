rm(list=ls())

# Challenger example from Faraway
library(faraway)
data(orings)
?orings

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim = c(0,1), xlab="Temperature", ylab="Probability of damage")
lmod1 <- lm(damage/6 ~ temp, orings)
abline(lmod1)

logitmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial, orings)
summary(logitmod)

plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim = c(0,1), xlab="Temperature", ylab="Prob of damage")
x <- seq(25,85,1)
lines(x,ilogit(11.6630-0.2162*x))

probitmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial(link=probit), orings)
summary(probitmod)
lines(x,pnorm(5.5915-0.1058*x),lty=2)

ilogit(11.6630-0.2162*31)
pnorm(5.5915-0.1058*31)

