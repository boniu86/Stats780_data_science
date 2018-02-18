setwd("/Users/sharonmc/Desktop/780/Lectures/Week 4")
library(rpart) # for the tree
library(rattle) # for the nice picture of the tree
library(ggplot2)

## Iris data
ggplot(iris, aes(Species, Petal.Length,col=Species)) + geom_boxplot() +  xlab("")

dev.print(device=postscript,"iris2.eps",horizontal=FALSE)

iris_tree <- rpart(Species ~ ., data=iris, method="class")
# Plot decision tree
fancyRpartPlot(iris_tree, main="Classification Tree for the Iris Data")

boxplot(Petal.Length~Species, data=iris, main="Boxplot for the Iris Data")

# Classification application
iris_train <- c(sample(1:50, 35), sample(51:100, 35), sample(101:150, 35))

iris_tree2 <- rpart(Species ~ ., data = iris, subset = iris_train)
fancyRpartPlot(iris_tree2)

table(predict(iris_tree2, iris[-iris_train,], type = "class"), iris[-iris_train, "Species"])

# For the partitioned scatter plots
library(tree)
tree_iris_twovar <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
plot(tree_iris_twovar)
text(tree_iris_twovar,cex=0.8)

plot(iris$Petal.Width,iris$Sepal.Width,pch=as.numeric(iris$Species),col=as.numeric(iris$Species),ylab="Sepal Width",xlab="Petal Width")
partition.tree(tree_iris_twovar,label="Species",add=TRUE)

## Titanic data set from http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets
titanic <- read.csv("titanic.txt")
head(titanic)

str(titanic)

titanic$pclass<-factor(titanic$pclass,ordered=TRUE)
titanic$pclass
titanic$pclass<-factor(titanic$pclass,levels=c("3rd","2nd","1st"),ordered=TRUE)
titanic$pclass
titanic_tree <- rpart(survived ~ pclass+age+sex, data=titanic, method="class")

# Plot decision tree
fancyRpartPlot(titanic_tree, main="Classification Tree for the Titanic Data")
# Classification application
train_size<-floor(0.75*dim(titanic)[1])

titanic_train <- sample(dim(titanic)[1], train_size)

titanic_tree2 <- rpart(survived ~ pclass+age+sex, data=titanic, subset = titanic_train,method="class",minsplit=25)

fancyRpartPlot(titanic_tree2)
table(predict(titanic_tree2, titanic[-titanic_train,], type = "class"),titanic[-titanic_train, "survived"])

## Wine data
data(wine,package="pgmm")
head(wine)

wine_tree <- rpart(Type ~ ., data=wine, method="class")
# Plot decision tree
fancyRpartPlot(wine_tree, main="Classification Tree for the Wine Data")

# Classification application
wine_train <- sample(1:nrow(wine), size=0.75*nrow(wine))
wine_tree2 <- rpart(Type ~ ., data = wine, subset = wine_train, method="class")

fancyRpartPlot(wine_tree2)

table(predict(wine_tree2, wine[-wine_train,], type = "class"), wine[-wine_train, "Type"])

## Hitters data
data(Hitters,package="ISLR")
??Hitters

head(Hitters)

Hitters<-na.omit(Hitters)
Hitters$logSalary<-log(Hitters$Salary)

hist(Hitters$Salary)
hist(Hitters$logSalary)

tree_hitters1 <- tree(logSalary ~ Years + Hits, data = Hitters, mincut=50)

which(log(Hitters$Salary)<=5.5)->low
which(log(Hitters$Salary)<=6.75)->med
temp<-rep(2,length(Hitters$Salary))
temp[med]<-3
temp[low]<-4

plot(Hitters$Years,Hitters$Hits,ylab="Hits",xlab="Years",pch=temp,col=temp)
partition.tree(tree_hitters1,label="logSalary",add=TRUE)

plot(tree_hitters1)
text(tree_hitters1)