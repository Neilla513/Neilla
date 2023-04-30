########SUPERVISED MACHINE LEARNING(CLASSIFICATION) WITH CARET PACKAGE##########################
#############################################################
###Compiled by Olawale Awe, PhD.
############################################################################
#This Code will introduce you to using the caret package to obtain the confusion matrix
####Load packages
library(caret)
install.packages("mlbench")
library(mlbench)
library(psych)
library(ggplot2)
##### START BY DOING MACHINE LEARNING WITH AN IN-BUILT DATASET-PimaIndiansDiabetes####
data()###view various inbuilt data you can practice with
# load the inbuilt dataset PimaIndiansDiabetes
data(PimaIndiansDiabetes)
attach(PimaIndiansDiabetes)
pdata=PimaIndiansDiabetes
pdata #view the data
#####################PARTITION THE DATA########################################

ind <- sample(2, nrow(pdata), replace = T, prob = c(0.7, 0.3))
train <- pdata[ind==1,]
test <- pdata[ind==2,]
# prepare training scheme for cross-validation
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the LVQ model
set.seed(7)
modelLvq <- train(factor(diabetes)~., data=train, method="lvq", trControl=control)
modelLvq
predLVq=predict(modelLvq,newdata = test)
predLVq
confusionMatrix(predLVq,as.factor(test$diabetes))
plot(predLVq, col= rainbow(2))
# train the GBM model
library(gbm)
set.seed(7)
modelGbm <- train(diabetes~., data=train, method="gbm", trControl=control, verbose=FALSE)
modelGbm
predGbm=predict(modelGbm,newdata = test)
predGbm
confusionMatrix(predGbm,as.factor(test$diabetes))
# train the SVM model
library(kernlab)
set.seed(7)
modelSvm <- train(diabetes~., data=train, method="svmRadial", trControl=control)
modelSvm
predSvm=predict(modelSvm,newdata = test)
predSvm
confusionMatrix(predSvm,as.factor(test$diabetes))
######################################################################

####Naive Bayes
library(naivebayes)
modelnb <- train(diabetes~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
confusionMatrix(prednb,as.factor(test$diabetes))
######################################################################
#####Decision Tree
set.seed(7)
modelDT <- train(diabetes~.,
                 data = train, 
                 method = "rpart",
                 #metric = metric,
                 trControl = control)
modelDT
predDT=predict(modelDT,newdata = test)
predDT
confusionMatrix(predDT,as.factor(test$diabetes))
####################################################################
#############KNN MODEL
set.seed(7)
fit.knn <- train(diabetes~.,
                 data = train , 
                 method = "knn",
                 #metric = metric,
                 trControl = control)
fit.knn
predknn=predict(fit.knn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$diabetes))
# collect all resamples and compare
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, NB=modelnb, SVM=modelSvm, DT=modelDT,KNN=fit.knn))
results
# summarize the distributions of the results 
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
#####################################################################
#########################################################################
####Now repeat the entire process using your Practdata, choosing any target variable of your choice, 

###Compute  the Confusion matrix model using your data and do for both  muulti-class and binary cases.
###Choose bot binary and multi-class target variables
###Dont forget to first describe your daata  using the describe() command

#############################################################################
#Example
setwd("~/Documents/AIMS2223/REVIEW COURSES/BLOCK4/ML/notebook/MLR")
practdata=read.csv('Practdata.csv')
practdata #view the data
Income<-practdata$Income
Age<-practdata$Age
#####################PARTITION THE DATA########################################

ind <- sample(2, nrow(pdata), replace = T, prob = c(0.7, 0.3))
train <- pdata[ind==1,]
test <- pdata[ind==2,]


ind1 <- sample(2, nrow(practdata), replace = T, prob = c(0.7, 0.3)) ###you can also try 80 by 20
train1 <- practdata[ind1==1,]
test1 <- practdata[ind1==2,]

####Then begin to train various models and compute their confusion matrices
dim(train1)
dim(test1)

control1 <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the LVQ model
set.seed(7)
model1 <- train(factor(Age)~., data=train1, method="lvq", trControl=control1)
model1
pred1=predict(model1,newdata = test1)
pred1
confusionMatrix(pred,as.factor(test1$Age))
plot(pred1, col= rainbow(2))
###Tabulate your results

####Stop here
#####################################################################

