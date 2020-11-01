#########################################################################
# Multivariate Analysis
#
# Content:   Classification tree                  
#########################################################################
install.packages("caret")
Sys.setenv(LANG="en")
setwd("C:/Users/steam/Desktop/")

library(caret)
library(tree)
library(e1071)
library(party)
library(maptree)

#Preparing
df <- read.csv("credit.csv", header=T, fileEncoding = "UTF-8-BOM")
colnames(df)[colnames(df)=="default.payment.next.month"] = "default"

#Chaging data types
df$SEX<- as.factor(df$SEX)
df$EDUCATION<- as.factor(df$EDUCATION)
df$MARRIAGE<- as.factor(df$MARRIAGE)
df$default<- as.factor(df$default)
summary(df)

#Train-Test-Set
set.seed(1000)
intrain <- createDataPartition(y=df$default, p=0.7, list=FALSE)
train <- df[intrain, ] #70% for train
test <- df[-intrain, ] #30% for test



#-------------------------------------------------------------------------------
#Using Gini-index in tree growing
treemod_gini <- tree(default ~.-ID, split="gini", data = train,
                     control = tree.control(nobs=nrow(train), mincut=1000, 
                                            minsize=2000, mindev = 0))
plot(treemod_gini) #plot the tree
text(treemod_gini) #text in the tree
summary(treemod_gini) 

#Cross-validation
cv_tree_gini <- cv.tree(treemod_gini, FUN = prune.tree, K=500) 
plot(cv_tree_gini, 
     cex.lab=1.8, 
     cex.axis=1.8,
     xlab = "Size of the Classification Tree",
     ylab = "Deviance")

plot(cv_tree_gini$size, cv_tree_gini$dev, type = "b",
     xlab = "Tree Size", ylab = "CV Misclassification Rate",
     cex.lab=1.8, 
     cex.axis=1.8)



#Pruning the tree
prune_trees_gini <- prune.tree(treemod_gini,best = 5)
plot(prune_trees_gini)
text(prune_trees_gini, pretty = 0)
summary(prune_trees_gini)


#make nicer
draw.tree (prune_trees_gini, 
           nodeinfo=TRUE, cases="", print.levels=TRUE,new=TRUE)


#predict of draft tree
treepred_gini_draft<- predict(treemod_gini, test, type = 'class') #test
confusionMatrix(treepred_gini, test$default)


#predict of pruned tree
treepred_gini <- predict(prune_trees_gini, test, type = 'class') #test
confusionMatrix(treepred_gini, test$default)

