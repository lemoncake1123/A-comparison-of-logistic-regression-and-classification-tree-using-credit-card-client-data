#########################################################################
# Multivariate Analysis
#
# Content:   Random Forests                     
#########################################################################



rm(list=ls())
Sys.setenv(LANG="en")



# Loading libraries and the data
library(rpart)
library(partykit)
library(caret)
library(forecast)
library(randomForest)



setwd("C:/Users/steam/Desktop/")
Daten <- read.csv("credit.csv", header=T, fileEncoding = "UTF-8-BOM")
colnames(Daten)[colnames(Daten)=="default.payment.next.month"] = "default"



summary(Daten)



#Changing data type
Daten$SEX<- as.factor(Daten$SEX)
Daten$EDUCATION<- as.factor(Daten$EDUCATION)
Daten$MARRIAGE<- as.factor(Daten$MARRIAGE)
Daten$default<- as.factor(Daten$default)



# Split data in two partitions (70% Training, 30% Validation)
set.seed(10)
inTrain <- createDataPartition(Daten$default, p = 0.7, list = FALSE)
train <- Daten[inTrain, ]
vali <- Daten[-inTrain,]





#-----------------------------------------------------------
# Random forest with classification with all variables
set.seed(1)
fit<-randomForest(default~.-ID,data=Daten,mtry=sqrt(23), 
                  importance =TRUE,subset=inTrain)
fit
plot(fit)



#confusion matrix
a <- fit$confusion
sum(diag(a))/sum(a)
summary(fit)



#make nicer
layout(matrix(c(1,2),nrow=1),width=c(4,1)) 
par(mar=c(5,4,4,0))




# Importance of each variable
fit$importance

a <- importance(fit)
library(xtable)
print(xtable(a, caption = 'Importance of each variable'), caption.placement = "bottom")



# Plot the importance of each variable
varImpPlot(fit,main="Importance of each variable")



# make nicer 
test<-sort(importance(fit)[,1])/max(importance(fit)[,1])
test<-data.frame(x1=labels(test),y1=test)
test<-transform(test, x1 = reorder(x1, y1))




ggplot(data=test, aes(x=x1, y=y1)) + ylab("Variable Importance Score") + xlab("") +
  geom_bar(stat="identity",fill="skyblue",alpha=1,width=.75) + 
  coord_flip()+
  theme(axis.text = element_text(size = 15))  +
  theme(text = element_text(size = 15))    





# Compare the performance of the bagged decision tree on the training and validation data
pred_t_ranFor<-predict(fit,newdata=train)
pred_v_ranFor<-predict(fit,newdata=vali)

confusionMatrix(pred_t_ranFor,train$default)
confusionMatrix(pred_v_ranFor,vali$default)
