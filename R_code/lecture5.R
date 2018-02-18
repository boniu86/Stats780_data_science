# Father/son height
data(father.son, package="UsingR")
ggplot(father.son, aes(fheight, sheight)) + geom_point() +
       geom_smooth(method="lm", colour="red") +
       xlab("Father's height (inches)")+ylab("Son's height (inches)")
       
model<-lm(sheight~fheight, father.son)
summary(model)

# mfrow=c(nrows, ncols) to create a matrix of nrows x ncols plots that are filled in by row
#mar – A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. 
par(mfrow=c(2,2),mar=c(5, 4, 4, 2) + 0.1)
plot(model)

# Trees data
pairs(trees, panel = panel.smooth, main = "trees data")
trees1 <- lm(Volume ~ Girth, data = trees)

summary(trees1)
plot(trees1)

trees2 <- lm(log(Volume) ~ log(Girth), data = trees)
summary(trees2)

plot(trees2)

trees3 <- update(trees2, ~ . + log(Height), data = trees)
summary(trees3)

plot(trees3)
