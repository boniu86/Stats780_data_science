library(cluster)



user<-biopsy[,-1]
user[,-10]<-lapply(user[,-10], as.numeric)
user<-na.omit(user)
x<-user[,-10]


hc <- hclust(dist(x), "single")
plot(hc)
l1<-cutree(hc,2)
table(unlist(user[,10]),l1)

hc2 <- hclust(dist(x), "complete")
plot(hc2)
l2<-cutree(hc2,2)
table(unlist(user[,10]),l2)

hc3 <- hclust(dist(x), "average")
plot(hc3)
l3<-cutree(hc3,2)
table(unlist(user[,10]),l3)



hc4 <- hclust(dist(x), "ward.D")
plot(hc4)
l4<-cutree(hc4,2)
table(unlist(user[,10]),l4)




library(cluster)
data(votes.repub)
?votes.repub

?diana

divisive <- diana(x, metric = "euclidean", stand = TRUE)
plot(divisive)


dv2 <- cutree(as.hclust(divisive), k = 2)
table(dv2) # 8 and 42 group members



#slide 22
## k-means and k-medoids clustering


user_kmeans<-kmeans(scale(user[,-10]),2)
user_kmeans
user_kmeans$cluster

table(unlist(user[,10]),user_kmeans$cluster)


si2 <- silhouette(user_kmeans$cluster, dist(scale(user[,-10])))
plot(si2, nmax= 80, cex.names=0.6)




user_kmedoids<-pam(scale(user[,-10]),2) 
user_kmedoids
user_kmedoids$clustering

table(unlist(user[,10]),user_kmedoids$clustering)

si3 <- silhouette(user_kmedoids$clustering, dist(scale(user[,-10])))
plot(si3, nmax= 80, cex.names=0.6)

par(mfrow=c(1,3))# This lets me put two figues on one row; good for comparing
plot(si2, nmax= 80, cex.names=0.6)
plot(si3, nmax= 80, cex.names=0.6)

# This is one way to produce an elbow plot
par(mfrow=c(1,1))# Back to displaying a single figure
x<-scale(user[,-10])
K<-15
wss<-rep(0,K)
for (k in 1:K){
  wss[k] <- sum(kmeans(x,k)$withinss)
}
plot(1:K,wss,typ="b",ylab="Total within cluster sum of squares",xlab="Number of clusters (k)")



library(mclust)
mod1 = Mclust(user[,-10])
summary(mod1)

plot(mod1,  what = c("BIC", "classification"))


mod2 = Mclust(user[,-10])
summary(mod2)
plot(mod2,  what = c("classification"))

tab<-table(unlist(user[,10]),map(mod2$z))

mod2a = Mclust(user[,-10],2:2)
summary(mod2a)
plot(mod2a,  what = c("classification"))
tab<-table(unlist(user[,10]),map(mod2a$z))
classAgreement(tab)
tab






