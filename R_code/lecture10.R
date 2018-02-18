# Efron & Tibshirani, Table 2.1
x<-c(10,27,31,40,46,50,52,104,146)
n<-length(x)
mean(x)
sd(x)
set.seed(1234)
M<-1000
temp<-rep(0,M)
for(i in 1:M){
	temp[i]<-median(sample(x,n,replace=TRUE))
}
# Bootstrat estimate of median
mean(temp)
# Actual median
median(x)
# This is SE_boot
sd(temp)
# This is the direct computation of SE median
diff<-0
for(i in 1:n){
	diff<-diff+(x[i]-median(x))^2	
}
sqrt(diff/(n-1))/sqrt(n+2)
# Bias
mean(temp)-median(x)
#Using library boot
library(boot)
set.seed(1234)
med<-function(y,i){median(y[i])}
boot(x,med,M)

# Bootstrap PI example
#s1<-floor(runif(1,0,9999999))
s1<-43340
set.seed(s1)
pop1<-c(rep(1,119),rep(0,10918))
pop2<-c(rep(1,98),rep(0,10936))
M<-1000
theta_ast<-rep(0,M)
for(i in 1:M){
	temp1<-sample(pop1,11037,replace=TRUE)
	temp2<-sample(pop2,11034,replace=TRUE)
	theta_ast[i]<-(sum(temp1)/11037)/(sum(temp2)/11034)
}
sort(theta_ast)[0.025*M]
sort(theta_ast)[0.975*M]

library(ggplot2)
theta_frame <- data.frame(theta_ast)
p <- ggplot(theta_frame, aes(x = theta_ast))
p <- p + geom_histogram(aes(y=..density..), binwidth=0.02, colour="black", fill="white") + geom_density(alpha=0.2, fill="#6666FF")
p<-	p + geom_vline(aes(xintercept=sort(theta_ast)[0.025*M]), colour="#0000AA", linetype="longdash")
p<-	p + geom_vline(aes(xintercept=sort(theta_ast)[0.975*M]), colour="#0000AA", linetype="longdash")
p