library(readr)
library(tidyverse)
library(e1071)


liver_data<-read_csv("Indian_Liver_Patient_Dataset.csv",col_names = FALSE)
x = as.data.frame(liver_data[,c(1,2,3,8,10,11)])
colnames(x) = c("Age","Gender","Total_Bilirubin", "Total_Protein", "AG_Ratio", "YesNo")
x = subset(x, Age >= 20) # take ages greater than 20
x = na.omit(x) #Exclude rows with NA
(x%>%mutate(Gender=factor(Gender), YesNo=factor(YesNo))
)->x1
model = glm( YesNo  ~ ., binomial(link='logit'), x1)
summary(model)


model2 = glm( YesNo  ~ Age + Total_Bilirubin + AG_Ratio , binomial(link='logit'), x1)
summary(model2)

G1<-562.91-561.14
pchisq(G1,2,lower.tail = FALSE)


train.indices = sample(1:nrow(x1), size=0.7*nrow(x1))
test = x1[-train.indices,]
train = x1[train.indices,]
dim(test)
dim(train)
x1.train=glm(YesNo  ~ . , binomial(link='logit'), train)
summary(x1.train)
pred_classes<-round(predict(x1.train,newdata=test,type="response"))
tab1 = table(test[,6],pred_classes)
tab1


x1.train=glm(YesNo  ~ Age + Total_Bilirubin + AG_Ratio , binomial(link='logit'), train)
summary(x1.train)
pred_classes<-round(predict(x1.train,newdata=test,type="response"))
tab2 = table(test[,6],pred_classes)
tab2

classAgreement(tab1)
classAgreement(tab2)

