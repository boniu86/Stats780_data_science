# Examples (mostly) from Unwin (2015)
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 2")
library(ggplot2)
library("ggthemes")
library("GGally")
library("extracat")
library(hdrcde)
library(KernSmooth)
library("gridExtra")
library("GDAdata")
library("reshape")

# Two bivariate Gaussians -- this one from McNicholas (2016, ch. 9)
load("mcnicholas.Rdata")
ggplot(df,aes(x=v1,y=v2))+ geom_point(alpha = 1)+theme_bw()+xlab("Variable 1")+ylab("Variable 2") +
  theme(
    panel.grid.major = element_line(size = 0.5, colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(colour = NA),
    axis.ticks = element_line(colour = "black")
  )
  
ggplot(df,aes(x=v1,y=v2))+ geom_point(alpha = 0.125)+theme_bw()+xlab("Variable 1")+ylab("Variable 2") +
  theme(
    panel.grid.major = element_line(size = 0.5, colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(colour = NA),
    axis.ticks = element_line(colour = "black")
  )
  
ggplot(df,aes(x=v1,y=v2))+ geom_point(alpha = 0.125)+geom_density2d(colour="pink")+theme_bw()+xlab("Variable 1")+ylab("Variable 2") +
  theme(
    panel.grid.major = element_line(size = 0.5, colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(colour = NA),
    axis.ticks = element_line(colour = "black")
  )
  
ggplot(df,aes(x=v1,y=v2))+ geom_point(alpha = 0.125)+stat_density2d(aes(fill = ..level..), geom="polygon")+scale_fill_gradient(low = "gray90", high = "blue")+theme_bw() +xlab("Variable 1")+ylab("Variable 2")+
  theme(
    panel.grid.major = element_line(size = 0.5, colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(colour = NA),
    axis.ticks = element_line(colour = "black")
  )

# Fathers and sons 
data(father.son, package="UsingR")
c2 <- ggplot(father.son, aes(sheight)) + 
             geom_histogram(aes(y = ..density..), binwidth=1) +
             geom_density(colour="blue") + xlim(58, 80) + ylim(0, 0.16) +
             xlab("Height (inches)") + ylab("") + ggtitle("Sons")
p2 <- ggplot(father.son, aes(fheight)) + 
             geom_histogram(aes(y = ..density..), binwidth=1) +
             geom_density(colour="blue") + xlim(58, 80) + ylim(0, 0.16) +
             xlab("Height (inches)") + ylab("") +
             ggtitle("Fathers")
grid.arrange(c2, p2, nrow = 1)

qqnorm(father.son$sheight, main="Sons", xlab="",ylab="", pch=16, ylim=c(55,80))
qqline(father.son$sheight)

qqnorm(father.son$fheight, main="Fathers", xlab="",ylab="", pch=16, ylim=c(55,80))
qqline(father.son$fheight)

ggplot(father.son, aes(fheight, sheight)) + geom_point() +
       geom_smooth(method="lm", colour="red", se=FALSE) +
       geom_abline(slope=1, intercept=0) +xlab("Father's height (inches)")+ylab("Son's height (inches)")
       
ggplot(father.son, aes(fheight, sheight)) + geom_point() +
       geom_smooth(method="lm", colour="red", se=TRUE) +
       geom_abline(slope=1, intercept=0) +xlab("Father's height (inches)")+ylab("Son's height (inches)")
       
ggplot(father.son, aes(fheight, sheight)) + geom_point() +
       geom_smooth(method="lm", colour="red", se=FALSE) +
       stat_smooth() +xlab("Father's height (inches)")+ylab("Son's height (inches)")
          
# Iris data
data(iris)
ggpairs(iris, aes(colour=Species, alpha=0.4))   
        
ggparcoord(iris, columns=1:4, groupColumn="Species")

a <- ggplot(iris, aes("Boxplot for all", Sepal.Width)) +
            xlab("")  + geom_boxplot() +
            scale_x_discrete(breaks=NULL) 
b <- ggplot(iris, aes(Species, Sepal.Width)) + 
            geom_boxplot() +  xlab("")
grid.arrange(a, b, nrow=1, widths=c(1,2))

a <- ggplot(iris, aes("Boxplot for all", Petal.Width)) +
            xlab("")  + geom_boxplot() +
            scale_x_discrete(breaks=NULL) 
b <- ggplot(iris, aes(Species, Petal.Width)) + 
            geom_boxplot() +  xlab("")
grid.arrange(a, b, nrow=1, widths=c(1,2))

# Body data
data(body, package="gclus")
body1 <- body
head(body1)

names(body1) <- abbreviate(names(body), 2)
names(body1)

names(body1)[c(4:5, 11:13, 19:21)] <-  
       c("CDp", "CD", "Ch", "Ws", "Ab", "Cl", "An", "Wr")
       
       #R thinks gender is a continuous random variable
a1 <- ggparcoord(body1, columns=1:24, alphaLines=0.1,groupColumn="Gn") + xlab("") + ylab("")
a2 <- ggparcoord(body1, columns=1:24, scale="uniminmax", alphaLines=0.1) + xlab("") + ylab("")
a3 <- ggparcoord(body1, columns=1:24, scale="globalminmax", alphaLines=0.1) + xlab("") + ylab("")
a4 <- ggparcoord(body1, columns=1:24, scale="center",scaleSummary="median", alphaLines=0.1) +xlab("") + ylab("")
grid.arrange(a1, a2, a3, a4)

 #uniminmax: univariately, scale so the minimum of the variable is zero, and the maximum is one
 #globalminmax: no scaling is done; the range of the graphs is defined by the global minimum and the global maximum
 #center: use uniminmax to standardize vertical height, then center each variable at a value specified by the scaleSummary param 
	# scaleSummary: if scale=="center", summary statistic to univariately center each variable by

#R thinks gender is a continuous random variable
a1 <- ggparcoord(body1, columns=1:24, alphaLines=0.1,groupColumn="Gn") + xlab("") + ylab("")
a1

#make gender a factor, and increase line thickness for easy reading
body1$Gn<-as.factor(body1$Gn)
a1 <- ggparcoord(body1, columns=1:24, alphaLines=0.3,groupColumn="Gn") +xlab("") + ylab("")
a1


a1 <- ggparcoord(body1, columns=1:24, alphaLines=0.1,groupColumn="Gn") + xlab("") + ylab("")
a2 <- ggparcoord(body1, columns=1:24, scale="uniminmax",groupColumn="Gn",alphaLines=0.1) + xlab("") + ylab("")
a3 <- ggparcoord(body1, columns=1:24,scale="globalminmax", alphaLines=0.1,groupColumn="Gn") + xlab("") + ylab("")
a4 <- ggparcoord(body1, columns=1:24, scale="center",scaleSummary="median", alphaLines=0.1,groupColumn="Gn") +xlab("") + ylab("")
grid.arrange(a1, a2, a3, a4)

# Coffee data
data(coffee, package="pgmm")
coffee <- within(coffee, Type <- ifelse(Variety==1,"Arabica", "Robusta"))
names(coffee) <- abbreviate(names(coffee), 8)
ggpairs(coffee[,-c(1,2)], aes(colour=Type, alpha=0.4)) 
          
names(coffee)[6]<-"Ph"          
a <- ggplot(coffee, aes(x=Type)) + geom_bar(aes(fill=Type)) +
            scale_fill_manual(values = c("grey70", "red")) +
            guides(fill=FALSE) + ylab("")
b <- ggplot(coffee, aes(x=Fat, y=Caffine, colour=Type)) +
            geom_point(size=2) +
            scale_colour_manual(values = c("grey70", "red"))
c <- ggparcoord(coffee[order(coffee$Type),], columns=3:14,
                groupColumn="Type", scale="uniminmax") +
                xlab("") +  ylab("") +
                theme(legend.position = "none") +
                scale_colour_manual(values = c("grey","red")) +
                theme(axis.ticks.y = element_blank(),
                axis.text.y = element_blank())
grid.arrange(arrangeGrob(a, b, ncol=2, widths=c(1,2)), c, nrow=2)

# Housing data
library(vcd)    
data(housing, package="MASS")
?housing

mosaic(xtabs(Freq ~ Cont + Type + Infl + Sat, data = housing),
       direction = c("h", "v", "v", "h"), 
       gp = gpar(fill = c("grey", "grey","blue")),
       spacing = spacing_highlighting)

#extra colour, infl is now last       
mosaic(xtabs(Freq ~ Cont + Type + Sat + Infl, data = housing),
direction = c("h", "v", "v", "h"), 
gp = gpar(fill = c("red", "grey","blue")),
spacing = spacing_highlighting)
       

    
# Crime data
data(crime.us, package="VGAMdata")
head(crime.us)

crime1<-crime.us
names(crime1)<-gsub("*Rate","",names(crime1))
names(crime1)[19:20]<-c("Larceny","MVTheft")

ggpairs(crime1[,c(13:16,18:20)],diag=list(continuous="densityDiag"),axisLabels="none")           

