rm(list=ls())
setwd("/Users/sharonmc/Desktop/780/Lectures/Week 3")

### CHD data from Hosmer and Lemeshow (2000)
CHD_data<-read.csv("chd_data.csv")

#A data.frame with 100 rows and 4 variables: 
# id Identification code (1 - 100) # age Age (Years) 
#AGED: age<55(0), age>=55(1)
#chd Presence of CHD (1: No, 2: Yes) 
#chd=coronary heart disease

CHD_data


## X Continuous
CHD_blogreg = glm(CHD ~ AGE,data=CHD_data,family=binomial("logit"))
CHD_blogreg
summary(CHD_blogreg)

# Estimated odds for an increase of 10 years
exp(0.11092*10)

# Someone aged 50
pi_hat50<-exp(-5.30945+(0.11092*50))/(1+exp(-5.30945+(0.11092*50)))
pi_hat50

# Someone aged 62
pi_hat62<-exp(-5.30945+(0.11092*62))/(1+exp(-5.30945+(0.11092*62)))
pi_hat62

## X Binary, AGE dichotomized at 55 years 
CHD_blogreg2 = glm(CHD ~ AGED,data=CHD_data,family=binomial("logit"))
CHD_blogreg2
summary(CHD_blogreg2)

# Odds ratio - point estimate
or<-exp(2.0935)
or
# Odds ratio - 95% CI
or_lower<-exp(2.0935-(1.96*0.5285))
or_upper<-exp(2.0935+(1.96*0.5285))
or_lower
or_upper

### Menarche
data(menarche, package="MASS")
??menarche

head(menarche)

menarche[1:20,]
plot(Menarche/Total ~ Age, data=menarche)

menarche_logreg = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)
plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, menarche_logreg$fitted, type="l", col="blue")

summary(menarche_logreg)
confint.default(menarche_logreg)
# Odds ratio
#intercept
#exp(coef(menarche_logreg)) 
#exp(-21.22639)

exp(1.63197)
# for each one year increase in age, a girl is 5.11 times more likely to have reached menarche

exp(cbind(OR = coef(menarche_logreg), confint.default(menarche_logreg)))

############################################################################
#multiple predictors, Y is dichotomous (binary)
# Low Birthweight data from Hosmer and Lemeshow (2000)
lbwt_data<-read.csv("lowbwt.csv")
head(lbwt_data)

#0 = low birth weight. 1 = normal or high birth weight
#lwt = weight of mother just before pregnancy (at last menstruation)
#ftv = number of physician visits during firt trimester
#189 observations

lbwt_data$RACE <- factor(lbwt_data$RACE)

lbwt_blogreg = glm(LOW ~ AGE+LWT+RACE+FTV,data=lbwt_data,family=binomial("logit"))
summary(lbwt_blogreg)

#G = D(model without the variable) − D(model with the variable)
G<-234.67-222.57
G
pchisq(G,5,lower.tail=FALSE)
#=> at least one of the "slopes" is significantly different from zero

# reduced model (i.e., without AGE and FTV)
lbwt_blogreg2 = glm(LOW ~ LWT+RACE,data=lbwt_data,family=binomial("logit"))
summary(lbwt_blogreg2)

G<-223.26-222.57
G
pchisq(G,2,lower.tail=FALSE)
#=> reduced model (i.e., without AGE and FTV) as good as full model
# THE NULL HYPOTHESIS IS THAT THE FULL MODEL IS NO BETTER THAN THE REDUCED MODEL.

#remove race
lbwt_blogreg3 = glm(LOW ~ LWT,data=lbwt_data,family=binomial("logit"))
summary(lbwt_blogreg3)
G<-228.69-223.26
G
pchisq(G,2,lower.tail=FALSE)
#=> strictly, reduced model (i.e., without RACE) as good as model with RACE (at alpha=5%)
# However, other matters must be considered --- esp in such a marginal case
# Now, consider collapsing RACE to a binary variable (rather than excluding it)
length(lbwt_data$RACE)
lbwt_data$RACED<-rep(0,189)
lbwt_data$RACED[which(lbwt_data$RACE==2)]<-1
lbwt_data$RACED[which(lbwt_data$RACE==3)]<-1

lbwt_data$RACE
lbwt_data$RACED

lbwt_blogreg4 = glm(LOW ~ LWT+RACED,data=lbwt_data,family=binomial("logit"))
summary(lbwt_blogreg4)
G<-228.69-224.65
G
pchisq(G,1,lower.tail=FALSE)
#=> (compare with model with just LWT). More complicated model (i.e., with RACED) is better (but marginal again)

#####################################################################
#####################################################################
### Multinomial Logistic Regression
### Mammography example from Hosmer and Lemeshow (2000)


library(nnet)
library(foreign)
mam <- read.csv("mam_exp.csv")
head(mam)

str(mam)

mam$SYMPT <- factor(mam$SYMPT)
mam$HIST <- factor(mam$HIST)
mam$BSE <- factor(mam$BSE)
mam$DETC <- factor(mam$DETC)
mam_logreg <- multinom(ME ~ SYMPT + PB +HIST + BSE + DETC, data = mam)
summary(mam_logreg)
#ME has 3 levels ("mamogram experience")
#0 = never
#1 = within 1 year
#2 = over 1 year ago

#calculate p values
z <- summary(mam_logreg)$coefficients/summary(mam_logreg)$standard.errors
2*(1 - pnorm(abs(z), 0, 1))

# First, consider making SYMPT into a binary variable (agree / disagree)
length(mam$SYMPT)
mam$SYMPTD<-rep(1,412)
mam$SYMPTD[which(mam$SYMPT==1)]<-0
mam$SYMPTD[which(mam$SYMPT==2)]<-0
mam$SYMPT[1:100]
mam$SYMPTD[1:100]

mam_logreg2 <- multinom(ME ~ SYMPTD + PB +HIST + BSE + DETC, data = mam)
summary(mam_logreg2)

z <- summary(mam_logreg2)$coefficients/summary(mam_logreg2)$standard.errors
2*(1 - pnorm(abs(z), 0, 1))

G<-697.4959-693.9019
G
pchisq(G,4,lower.tail=FALSE)
#=> more complicated model is no better
# Next, consider removing DETC

mam_logreg3 <- multinom(ME ~ SYMPTD + PB +HIST + BSE, data = mam)
summary(mam_logreg3)
z <- summary(mam_logreg3)$coefficients/summary(mam_logreg3)$standard.errors
2*(1 - pnorm(abs(z), 0, 1))

G<-706.0381-697.4959
G
pchisq(G,4,lower.tail=FALSE)
# Could exclude DETC, but p is marginal...
 
# Now, consider collapsing DETC to a binary variable rather than excluding
# ... this one seemed sensible to me (I'm not a subject matter expert!!)
mam$DETCD<-rep(1,412)
mam$DETCD[which(mam$DETC==1)]<-0
mam$DETCD

mam_logreg4 <- multinom(ME ~ SYMPTD + PB +HIST + BSE + DETCD, data = mam)
summary(mam_logreg4)

z <- summary(mam_logreg4)$coefficients/summary(mam_logreg4)$standard.errors
2*(1 - pnorm(abs(z), 0, 1))

G<-706.0381-703.7606
G

pchisq(G,2,lower.tail=FALSE)

#odds ratio, for final model
exp(coef(mam_logreg4))

# Now, consider an alternative dichotomy (binary variable) for DETCD
# ... I think this one is actually more sensible
mam$DETCD2<-rep(1,412)
mam$DETCD2[which(mam$DETC==1)]<-0
mam$DETCD2[which(mam$DETC==2)]<-0
mam$DETCD2

mam_logreg5 <- multinom(ME ~ SYMPTD + PB +HIST + BSE + DETCD2, data = mam)
summary(mam_logreg5)

z <- summary(mam_logreg5)$coefficients/summary(mam_logreg4)$standard.errors
2*(1 - pnorm(abs(z), 0, 1))

G<-706.0381-699.1326
G

pchisq(G,2,lower.tail=FALSE)
#=> DETCD2 is making a signifcant contribution
exp(coef(mam_logreg5))

#install.packages("ElemStatLearn")
# South African heart disease data
data(SAheart,package="ElemStatLearn")
??SAheart

head(SAheart)

train.indices = sample(1:nrow(SAheart), size=0.9*nrow(SAheart))
SAheart.test = SAheart[-train.indices,]
SAheart.train = SAheart[train.indices,]
dim(SAheart.test)
dim(SAheart.train)
heart_logreg=glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,data=SAheart.train,family=binomial("logit"))
summary(heart_logreg)

pred_classes<-round(predict(heart_logreg,newdata=SAheart.test,type="response"))
table(SAheart.test[,10],pred_classes)

# Now, the reduced model
heart_logreg2=glm(chd~tobacco+ldl+famhist+age,data=SAheart.train,family=binomial("logit"))
summary(heart_logreg2)

pred_classes2<-round(predict(heart_logreg2,newdata=SAheart.test,type="response"))
tab<-table(SAheart.test[,10],pred_classes2)
tab

library(e1071)
classAgreement(tab)
