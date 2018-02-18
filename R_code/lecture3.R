### Much of this is taken -- with or without modification -- from Unwin (2015).
rm(list=ls())
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 2")
library("ggthemes")
library("GGally")
library("extracat")
library(hdrcde)
library(KernSmooth)
library("ggplot2")
library("gridExtra")
library("vcd")


# The Titanic data revisited
Titanic1 <- data.frame(Titanic)
head(Titanic1)
str(Titanic1)
p <- ggplot(Titanic1, aes(weight=Freq)) + ylab("") + ylim(0,2250)
cs <- p + aes(Class) + geom_bar(fill="blue")
sx <- p + aes(Sex) + geom_bar(fill="green")
ag <- p + aes(Age) + geom_bar(fill="tan2")
su <- p + aes(Survived) + geom_bar(fill="red")
grid.arrange(cs, sx, ag, su, nrow=1, widths=c(3, 2, 2, 2))

doubledecker(Survived ~ Sex, data = Titanic, gp = gpar(fill = c("grey90", "blue")))

doubledecker(Survived ~ Class, data = Titanic, gp = gpar(fill = c("grey90", "blue")))

doubledecker(Survived ~ Sex + Class, data = Titanic, gp = gpar(fill = c("grey90", "blue")))

doubledecker(Survived ~ Class + Age, data = Titanic, gp = gpar(fill = c("grey90", "blue")))

doubledecker(Survived ~ Sex + Class + Age, data = Titanic, gp = gpar(fill = c("grey90", "blue")))

# Movies data
#install.packages("ggplot2movies")
library(ggplot2movies)
data(movies)
?movies

ggplot(movies, aes(length)) + geom_bar() + ylab("") + xlab("Movie length (minutes)")

ggplot(movies, aes("var", length)) + geom_boxplot() + xlab("") +
       ylab("Movie length (minutes)")  + scale_x_discrete(breaks=NULL) + coord_flip()
       
ggplot(movies, aes(length)) + ylab("") +  xlim(0,180) +
       geom_histogram(binwidth=1)  +
       xlab("Movie length (minutes)")
       
ggplot(movies, aes(x = length)) +  xlim(0,240) +
       geom_histogram(binwidth=1)  +
       xlab("Movie length (minutes)") + ylab("")
       
ggplot(movies, aes(x = length)) +  xlim(0,240) +
       geom_histogram(aes(y=..density..),binwidth=1)  +
       xlab("Movie length (minutes)") + ylab("") +geom_density(colour="blue")
       
ggplot(movies, aes(votes, rating,alpha=0.005)) + geom_point(size=0.25) + ylim(1,10) + ylab("Rating") + xlab("Votes")

foo<-which(movies$votes>10000)
length(foo)

ggplot(movies[foo,], aes(votes, rating)) + geom_point() + ylim(1,10) + ylab("Rating") + xlab("Votes")

foo1<-which(movies$rating[foo]==min(movies$rating[foo]))
movies1<-movies[foo,]
movies1[foo1,]

foo2<-which(movies$rating[foo]==max(movies$rating[foo]))
movies2<-movies[foo,]
movies2[foo2,]

temp<-sort(movies2$rating,decreasing=TRUE,index.return=TRUE)
temp$ix
movies2[temp$ix,]

movies2[temp$ix,1]

summary(movies$year)



# Berkeley data
ucba <- as.data.frame(UCBAdmissions)
a <- ggplot(ucba, aes(Dept)) + geom_bar(aes(weight=Freq))
b <- ggplot(ucba, aes(Gender)) + geom_bar(aes(weight=Freq))
c <- ggplot(ucba, aes(Admit)) + geom_bar(aes(weight=Freq))
grid.arrange(a, b, c, nrow=1, widths=c(7,3,3))

ucba <- within(ucba, Accept <- factor(Admit, levels=c("Rejected", "Admitted")))
doubledecker(xtabs(Freq~ Dept + Gender + Accept, data = ucba), gp = gpar(fill = c("grey90", "steelblue")))

#Geyser Data
library(hdrcde)
??hdrcde

data(geyser, package="MASS")
ggplot(geyser, aes(duration, waiting)) + geom_point() + ylab("Waiting time (minutes)") + xlab("Duration (minutes)")

ggplot(geyser, aes(duration, waiting)) + geom_point() + geom_density2d() + ylab("Waiting time (minutes)") + xlab("Duration (minutes)")