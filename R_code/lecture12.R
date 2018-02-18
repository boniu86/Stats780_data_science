

## Hierarchical clustering
?hclust

#slide 10
# US cities exmample from hclust help; see it for details
data(UScitiesD)
?UScitiesD

hcity.D2 <- hclust(UScitiesD, "ward.D2")
plot(hcity.D2, hang=-1)
#hang
#The fraction of the plot height by which labels should hang below the rest of the plot. A negative value #will cause the labels to hang down from 0.

# Coffee data
data(coffee,package="pgmm")
head(coffee)
??coffee
x<-scale(coffee[,-c(1,2)])
hc <- hclust(dist(x), "single")
plot(hc)
hc2 <- hclust(dist(x), "complete")
plot(hc2)
hc3 <- hclust(dist(x), "average")
plot(hc3)
hc4 <- hclust(dist(x), "ward.D")
plot(hc4)
l4<-cutree(hc4,3)
table(coffee[,1],l4)

l5<-cutree(hc4,2)
table(coffee[,1],l5)

# Wine data from pgmm
data(wine,package="pgmm")
head(wine)
x<-scale(wine[,-c(1)])
hc <- hclust(dist(x), "single")
plot(hc)
l1<-cutree(hc,3)
table(wine[,1],l1)

hc2 <- hclust(dist(x), "complete")
plot(hc2)
l2<-cutree(hc2,3)
table(wine[,1],l2)

hc3 <- hclust(dist(x), "average")
plot(hc3)
l3<-cutree(hc3,3)
table(wine[,1],l3)

hc4 <- hclust(dist(x), "ward.D")
plot(hc4)
l4<-cutree(hc4,3)
table(wine[,1],l4)

#divisive
library(cluster)
data(votes.repub)
?votes.repub

?diana

divisive <- diana(votes.repub, metric = "euclidean", stand = TRUE)
plot(divisive)


dv2 <- cutree(as.hclust(divisive), k = 2)
table(dv2) # 8 and 42 group members
rownames(votes.repub)[dv2 == 1]

#To divide the selected cluster, the algorithm first looks for its most disparate observation (i.e., which has the largest average dissimilarity to the other observations of the selected cluster). This observation initiates the "splinter group". In subsequent steps, the algorithm reassigns observations that are closer to the "splinter group"

#slide 22
## k-means and k-medoids clustering

# Iris example
library(cluster)
data(iris)
head(iris)

x<-iris[,-5]
iris_kmeans<-kmeans(x,3)

plot(x, col = iris_kmeans$cluster)

table(iris[,5],iris_kmeans$cluster)

?silhouette

si1 <- silhouette(iris_kmeans$cluster, dist(x))
summary(si1)
plot(si1, nmax= 80, cex.names=0.6)

# Coffee example
library(cluster)
data(coffee,package="pgmm")
head(coffee)

??coffee

coffee_kmeans<-kmeans(scale(coffee[,-c(1,2)]),2)
coffee_kmeans
coffee_kmeans$cluster

table(coffee[,1],coffee_kmeans$cluster)

si2 <- silhouette(coffee_kmeans$cluster, dist(scale(coffee[,-c(1,2)])))
plot(si2, nmax= 80, cex.names=0.6)

coffee_kmedoids<-pam(scale(coffee[,-c(1,2)]),2) 
coffee_kmedoids
coffee_kmedoids$clustering

table(coffee[,1],coffee_kmedoids$clustering)

si3 <- silhouette(coffee_kmedoids$clustering, dist(scale(coffee[,-c(1,2)])))
plot(si3, nmax= 80, cex.names=0.6)

par(mfrow=c(1,2))# This lets me put two figues on one row; good for comparing
plot(si2, nmax= 80, cex.names=0.6)
plot(si3, nmax= 80, cex.names=0.6)

# This is one way to produce an elbow plot
par(mfrow=c(1,1))# Back to displaying a single figure
x<-scale(coffee[,-c(1,2)])
K<-8
wss<-rep(0,K)
for (k in 1:K){
	wss[k] <- sum(kmeans(x,k)$withinss)
}
plot(1:K,wss,typ="b",ylab="Total within cluster sum of squares",xlab="Number of clusters (k)")
