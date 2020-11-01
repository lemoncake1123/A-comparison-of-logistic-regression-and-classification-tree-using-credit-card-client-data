#########################################################################
# Multivariate Analysis
#
# Content:   Logistic regression                    
#########################################################################
library(car)
library(ROCR)
library(xtable)
library(party)
citation(package = "party")
install.packages("ROCR")

Sys.setenv(LANG="en")
setwd("C:/Users/steam/Desktop/")
credit_data <- read.csv("credit.csv", header=T, fileEncoding = "UTF-8-BOM")
str(credit_data)


#Renaiming column, because its name is too long to use
colnames(credit_data)[colnames(credit_data)=="default.payment.next.month"] = "default"


#Chainging data type
credit_data$SEX<- as.factor(credit_data$SEX)
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE)
credit_data$default<- as.factor(credit_data$default)


#Find missing data
table(is.na(credit_data)) #no missing data



#-----------------------------------------------------------------------
#Exploratory Data Analysis

# Histogram for LIMIT_BAL variable
hist(credit_data$LIMIT_BAL, breaks = 50,
     main = " ",
     xlab = "LIMIT_BAL",
     ylab = "Number of Customers",
     cex.lab=1.8,
     cex.axis=1.8)


#Bar plot between default and sex
barplot(table(credit_data$default), 
        col="grey",
        xlab="Number of Customers",
        ylim=c(0,25000),
        names.arg=c("Non-Defaulter","Defaulter"),
        main="")
bar <- table(credit_data$SEX, credit_data$default)

#Pie graph between default and education level
pie(bar, main="Default with education variable", init.angle=90,
    col=rainbow(length(bar)),
    labels = c("no edu", "graduate school", "university", 
               "high school", "others", "unknown", "unknown"))
legend(1,1, c("no edu", "graduate school", "university", "high school", "others", "unknown", "unknown",
       cex=0.8, fill = rainbow(length(bar))))

#Pie graph between default and marriage 
pie(bar, main="Default with marriage variable", init.angle=90,
    col=rainbow(length(bar)),
    labels = c("unknown", "married", "single", "others"))
legend(1,1, c("unknown", "married", "single", "others"), 
       cex=0.8, fill = rainbow(length(bar)))



#-----------------------------------------------------------------------
#Logistic Regression Analysis

#Randomly split the data to training (70%) and testing (30%) datasets:
index <- sample(nrow(credit_data),nrow(credit_data)*0.70) #default=WOR
credit_train = credit_data[index,] #train-data=70%
credit_test = credit_data[-index,] #test-data=30%

#logistic regression model with all variables
credit_glm0<- glm(default ~ .-ID, data = credit_train, family = binomial)
summary(credit_glm0)   
AIC(credit_glm0)
BIC(credit_glm0)

#classification matrix of logistic regression model with all variables
yhat_1 <- predict(credit_glm0, credit_test, type = "response")
ypredict_1 <- ifelse(yhat_1>0.5,1,0)
tab_1 <- table(credit_test$default, ypredict_1)
sum(diag(tab_1))/sum(tab_1) #81,27%

#ROC graph of logistic regression model with all variables
p_glm0 <- predict(credit_glm0, newdata=credit_test, type="response")
pr_glm0 <- prediction(p_glm0, credit_test$default)
prf_glm0 <- performance(pr_glm0, measure = "tpr", x.measure = "fpr")
plot(prf_glm0) 

#AUC value of of logistic regression model with all variables
auc_glm0 <- performance(pr_glm0, measure = "auc")
auc_glm0 <- auc_glm0@y.values[[1]]
auc_glm0 


#For classification of output of logistic regression, a cut-off probability is required
#Cut off point
optid<-(1:length(prf_glm0@y.values[[1]][-1]))[((prf_glm0@x.values[[1]][-1])^2 
                                          + (1-prf_glm0@y.values[[1]][-11])^2)
                                         ==min((prf_glm0@x.values[[1]][-1])^2 
                                               + (1-prf_glm0@y.values[[1]][-1])^2)]
points(prf_glm0@x.values[[1]][-1][optid],prf_glm0@y.values[[1]][-1][optid], col='red', pch=15)
optcut<-prf_glm0@alpha.values[[1]][-1][optid]; optcut #0.2363
abc <- table(credit_test$default, p_glm0>0.2363)
sum(diag(abc))/sum(abc)

pred_glm0_test<- predict(credit_glm0, newdata = credit_test, type="response")
class_glm0_train<- (pred_glm0_test>0.2363)*1
abcd <- table(credit_test$default, class_glm0_train, dnn = c("True")); abcd
sum(diag(abcd))/sum(abcd)






#-----------------------------------------------------------------------
#Variable Selection with Stepwise Approach

#Backward selection
credit_glm_back <- step(credit_glm0, direction = "backward", trace = F) # backward selection (if you don't specify anything)
summary(credit_glm_back)
credit_glm_back$deviance
AIC(credit_glm_back)
BIC(credit_glm_back)

#Classification matrix of backward selection model
yhat_back <- predict(credit_glm_back, credit_test, type = "response")
ypredict_back <- ifelse(yhat_back>0.5,1,0)
tab_back <- table(credit_test$default, ypredict_back)
sum(diag(tab_back))/sum(tab_back) #81,22%

#ROC Curve of backward selection model
p <- predict(credit_glm_back, newdata=credit_test, type="response")
pr <- prediction(p, credit_test$default)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf) 

#AUC value of backward selection model
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc 


#-----------------------------------------------------------------------
#Forward selection
credit_glm0_min <- glm(default ~ 1, data = credit_data, family = binomial)
credit_glm_forwad <- step(credit_glm0_min, direction = "forward",
                          scope = (~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_0+PAY_2
                                   +PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+BILL_AMT2
                                   +BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1
                                   +PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6),
                          trace = F)
credit_glm_forwad$deviance
AIC(credit_glm_forwad)
BIC(credit_glm_forwad)

print(xtable(credit_glm_forwad, caption = 'Forward selection'), caption.placement = "bottom")

#Classification matrix of backward selection model
yhat_forw <- predict(credit_glm_forwad, credit_test, type = "response")
ypredict_forw <- ifelse(yhat_forw>0.5,1,0)
tab_forw <- table(credit_test$default, ypredict_forw)
sum(diag(tab_forw))/sum(tab_forw) #81,22%

#ROC curve of backward selection model
p_forw <- predict(credit_glm_forwad, newdata=credit_test, type="response")
pr_forw <- prediction(p_forw, credit_test$default)
prf_forw <- performance(pr_forw, measure = "tpr", x.measure = "fpr")
plot(prf_forw) 

#AUC value of backward selection model
auc_forw <- performance(pr_forw, measure = "auc")
auc_forw <- auc_forw@y.values[[1]]
auc_forw 

