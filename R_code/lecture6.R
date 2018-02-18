
# Fithful
pairs(faithful)

faithful.lm<-lm(eruptions ~ waiting, data=faithful)
summary(faithful.lm)

par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(faithful.lm)

# Trees
pairs(trees, panel = panel.smooth, main = "trees data")

plot(Volume ~ Girth, data = trees, log = "xy")
coplot(log(Volume) ~ log(Girth) | Height, data = trees,
       panel = panel.smooth)
       

###


# Gala
library(faraway)
data(gala, package="faraway")
??gala

head(gala)

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz  + Adjacent, data=gala)
summary(lmod)

par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(lmod)

lmod2 <- lm(sqrt(Species) ~ Area + Elevation + Nearest + Scruz  + Adjacent, data=gala)
summary(lmod2)
par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(lmod2)

library(MASS)
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
boxcox(lmod, lambda = seq(-0.25, 0.75, by = 0.05), plotit = T)

lmod3 <- lm(Species^(1/3) ~ Area + Elevation + Nearest + Scruz  + Adjacent, data=gala)
summary(lmod3)
plot(lmod3)

# Ozone
g <- lm(Ozone ~ Solar.R + Wind + Temp,airquality,na.action = na.exclude)
summary(g)

par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(g)
par(mfrow=c(1,1))
plot(fitted(g),residuals(g),xlab="Fitted",ylab="Residuals")

gl <- lm(log(Ozone) ~ Solar.R + Wind + Temp,airquality,na.action=na.exclude)
summary(gl)
par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(gl)

par(mfrow=c(1,1))
plot(fitted(gl),residuals(gl),xlab="Fitted",ylab="Residuals")
plot(residuals(gl),ylab="Residuals")

plot(residuals(gl)[-153],residuals(gl)[-1], xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0)


library(lmtest)
dwtest(Ozone ~ Solar.R + Wind + Temp,data=na.omit(airquality))

# State
data(state)
statedata <- data.frame(state.x77,row.names=state.abb)
g <- lm(Life.Exp ~ ., data=statedata)
summary(g)

par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(g)
g <- update(g, . ~ . - Area)
summary(g)
g <- update(g, . ~ . - Illiteracy)
summary(g)
g <- update(g, . ~ . - Income)
summary(g)
g <- update(g, . ~ . - Population)
summary(g)
g <- update(g, . ~ . + Population)
summary(g)
g <- lm(Life.Exp ~ ., data=statedata)
step(g)



