rm(list=ls())
setwd("~/Documents/AIMS2223/REVIEW COURSES/BLOCK4/ML/Assignments/Assign1")
wine<-read.csv("wine_dataset.csv", header=T)
dim(wine)
str(wine)
sum(is.na(wine))
sum(is.na(wine))


### we remove the target value which is quality
wine<-wine[,-c(13)]


## one of the features is not very relevant let use boruta
library(Boruta)
library(tidyr)
library(dplyr)
library(caret)#training
library(randomForest)
library(ggplot2)
library(lattice)
library(kernlab)
library(xtable)
library(tidyr)
#install.packages("randomForest")
#converting quality of wine as bad and good for classification

wine['target']<-ifelse(quality>5,'Good', "Bad")


# converting target to categorical
wine$target<-as.factor(wine$target)
wine<-wine[,-c(12)]
attach(wine)
summary(wine)
dim(wine)
#######################################################################################
ggplot(wine, aes(x=target, fill=target)) + geom_bar(stat = "count") +
  geom_text(position = "stack", stat='count', aes(label=..count..), vjust=-0.5) +
  labs(y="Observation", x="Wine Quality") +
  labs(title = "DIATRIBUTION OF RED WINE QUALITY")
######################################################################################################

wine %>% gather(target, key="var", value = "value") %>%
  ggplot(aes(x=target, y=value, color=target)) + geom_boxplot() +
  facet_wrap(~var, scales = "free", ncol = 3) + theme(legend.position = "none")

par(mfrow=c(3,4))
#boxplot(wine[,11], main="alcohol",plot=T)
## we observe we ahave some outliers,
#let's remove it
#fixed variable 1
par(mfrow=c(1,2))
boxplot(wine[,1], main='fixed_acidity')
outliers<-boxplot(wine[,1], plot = F)$out
wine<-wine[-which(wine[,1] %in% outliers),]
boxplot(wine[,1], main= 'fixed_acidity')
pa<-summary(wine)

############### we remove the outliers for the 2nd variable volatile aditivity

par(mfrow=c(1,2))
boxplot(wine[,2], main='volatile_acidity')
outliers<-boxplot(wine[,2], plot = F)$out
wine<-wine[-which(wine[,2] %in% outliers),]
boxplot(wine[,2], main= 'volatile_acidity')
summary(wine)


###############we remove for the 3rd variable
par(mfrow=c(1,2))
boxplot(wine[,3], main='citric_acid')
outliers<-boxplot(wine[,3], plot = F)$out
wine<-wine[-which(wine[,3] %in% outliers),]
boxplot(wine[,3], main= 'citric_acid')
summary(wine)


#########for the 4th variable
par(mfrow=c(1,2))
boxplot(wine[,4], main='residual_sugar')
outliers<-boxplot(wine[,4], plot = F)$out
wine<-wine[-which(wine[,4] %in% outliers),]

boxplot(wine[,4], main= 'residual_sugar')
summary(wine)

############for the 5th variable
par(mfrow=c(1,2))
boxplot(wine[,5], main='chloride')
outliers<-boxplot(wine[,5], plot = F)$out
wine<-wine[-which(wine[,5] %in% outliers),]

boxplot(wine[,5], main= 'chloride')
######6th variable
par(mfrow=c(1,2))
boxplot(wine[,6], main='free_sulfure_dioxyde')
outliers<-boxplot(wine[,6], plot = F)$out
wine<-wine[-which(wine[,6] %in% outliers),]
boxplot(wine[,6], main= 'free_sulfure_dioxyde')

#########7th variable 

par(mfrow=c(1,2))
boxplot(wine[,7], main='total_sulfure_dioxyde')
outliers<-boxplot(wine[,7], plot = F)$out
wine<-wine[-which(wine[,7] %in% outliers),]
boxplot(wine[,7], main= 'total_sulfure_dioxyde')

############################## for the 8th var density don't have outlier

par(mfrow=c(1,2))
boxplot(wine[,8], main='density')
outliers<-boxplot(wine[,8], plot = F)$out
wine<-wine[-which(wine[,8] %in% outliers),]
boxplot(wine[,8], main= 'densityr')
####################################for 9th ph, sulfate, alcohol


par(mfrow=c(1,2))
boxplot(wine[,9], main='ph')
outliers<-boxplot(wine[,9], plot = F)$out
wine<-wine[-which(wine[,9] %in% outliers),]
boxplot(wine[,9], main= 'ph')
###########################for 10th sulfate

par(mfrow=c(1,2))
boxplot(wine[,10], main='sulfate')
outliers<-boxplot(wine[,10], plot = F)$out
wine<-wine[-which(wine[,10] %in% outliers),]
boxplot(wine[,10], main= 'sulfate')

############for 11th variable alcohol
par(mfrow=c(1,2))
boxplot(wine[,11], main='alcohol')
outliers<-boxplot(wine[,11], plot = F)$out
wine<-wine[-which(wine[,11] %in% outliers),]
boxplot(wine[,11], main= 'acohol')


str(wine)
attach(wine)
wine$target<-as.factor(wine$target)
#clean_x<-clean %>% remove_empty(whic=c("rows"))
#clean_x<-clean %>% remove_empty(whic=c("cols"))
########## using recursive feature elimination##########################################

set.seed(98)

boruta<-Boruta(target~.,data = wine)
boruta
par(mfrow=c(1,1))
plot(boruta)
getSelectedAttributes(boruta, withTentative = F)
############features selection
## At the end we obtain 4596 observation
#######################################################
##feature selection  using 

### a splitting data
set.seed(98)
win <- sample(2, nrow(wine), replace = T, prob = c(0.7, 0.3))
train <- wine[win==1,]
test <- wine[win==2,]
dim(train)
dim(test)
str(wine)
unique(wine)
names(wine)
#[1] "fixed_acidity"        "volatile_acidity"     "citric_acid"         
#[4] "residual_sugar"       "chlorides"            "free_sulfur_dioxide" 
#[7] "total_sulfur_dioxide" "density"              "pH"                  
#[10] "sulphates"
nrow(wine)
ncol(wine)
#######################
train$target<-make.names(train$target)


set.seed(98)

train$target<-as.factor(train$target)

############let's check the proportions of the splitted data
## the split proportion is 70/30 percent
set.seed(38)
prop.table(table(wine$target))
#   Bad    Good 
#0.31745 0.68255 
prop.table(table(train$target))
#    Bad      Good 
#0.3223787 0.6776213 0.3266771 0.6733229 
 #    Bad      Good 
prop.table(table(test$target))
#     Bad      Good 
#0.3062099 0.6937901  0.2961898 0.7038102 
View(wine)
dim(wine)
## tuning parameter
control<-trainControl(method = "repeatedcv", number = 10, repeats=5)
control
##### SVM: linear
set.seed(50)
library(kernlab)
modelsvm<-train(target~., data = train, method ="svmRadial", trControl=control)
#modelSvm <- caret::train(target~., data=train, method="svmLinear", trControl=control)
modelsvm
#modelsvm
#Support Vector Machines with Radial Basis Function Kernel 

#3205 samples
#11 predictor
#2 classes: 'Bad', 'Good' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 5 times) 
#Summary of sample sizes: 2885, 2886, 2884, 2884, 2884, 2885, ... 
#Resampling results across tuning parameters:
  
 # C     Accuracy   Kappa    
#0.25  0.7752302  0.4473160
#0.50  0.7777259  0.4609746
#1.00  0.7809699  0.4745225

#Tuning parameter 'sigma' was held constant at a value of 0.06904061
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.06904061 and C = 1.

predsvm=predict(modelsvm,newdata = test)
#predsvm
a<-confusionMatrix(predsvm,as.factor(test$target))

#Confusion Matrix and Statistics

#Reference /Prediction Bad Good /Bad  230  136 /Good 182  843 /Accuracy : 0.7714/95% CI : (0.7484, 0.7932)
#No Information Rate : 0.7038          
#P-Value [Acc > NIR] : 9.259e-09       

#Kappa : 0.4333          

#Mcnemar's Test P-Value : 0.01162         
                                          
           # Sensitivity : 0.5583          
          #  Specificity : 0.8611          
        # Pos Pred Value : 0.6284          
       #  Neg Pred Value : 0.8224          
         #    Prevalence : 0.2962         
       #  Detection Rate : 0.1653          
   #Detection Prevalence : 0.2631          
     # Balanced Accuracy : 0.7097          
                                          
      # 'Positive' Class : Bad             
    m1<-a$byClass[c(1,2,5,7,11)]   
    library(xtable)
xtable(m1) 
##Logistic regression
set.seed(50)
modellr<-train(target~., data = train, method ="glmnet",trControl=control,family="binomial")
modellr

> modellr
glmnet 


predlr<-predict(modellr,test)
b<-confusionMatrix(test$target, predlr, mode ="everything", positive = "Good")
m2<-b$byclass[c(1,2,5,7,11)]
m2
b
      
####Random forest
library(caret)
modelrf<-caret::train(target~., data=train, method = "rf", trControl= control, replace= T)
modelrf
######################################################
library()
ran <- randomForest(factor(target)~., data = train, trControl= control)
ran
pre <- predict(ran,test)
pre
confusionMatrix(pre,factor(test$target))
####################################################

#modelrf<-caret::train(target~., data=train, method = "rf", trControl= control, replace= T)

### take Null because the positive class is Bad

library(stargazer)
stargazer(m5)
###################KNN ############

set.seed(49)
fit.knn <- train(target~.,
                 data = train , 
                 method = "knn",
                 #metric = metric,
                 trControl = control)
fit.knn


predknn=predict(fit.knn,newdata = test)
#predknn
d<-confusionMatrix(predknn,test$target)

m4<-d$byClass[c(1,2,5,7,11)]
m4

#######################NB#####################

library(naivebayes)
modelnb <- train(target~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
e<-confusionMatrix(prednb,test$target)
e
m5<-e$byClass[c(1,2,5,7,11)]
m5
e
m4
###########################################################LOGISTIC REG########################
## measuring accuracy
accuracy<-round(data.frame(SVM= a$overall[1],RF = c$overall[1], KNN= d$overall[1], NB = e$overall[1],LR= b$overall[1]),3)
accuracy
## scores measurement
scores<-round(as.data.frame(SVM=m1, RF=m3, KNN=m4, NB=m5, LR=m2),3)
SCORES<-data_frame(SVM=m1, RF=m3,KNN=m4, NB=m5,LR=m2)
SCORES
xtable(SCORES)
#SCORES<-data.matrix(tm)
xtable(SCORES, type="latex",file="filename.tex", digits = 3)
######################################################
ggplot(wine, aes(x=target, fill=target)) + geom_bar(stat = "count") +
  geom_text(position = "stack", stat='count', aes(label=..count..), vjust=-0.5) +
  labs(y="Observation", x="Wine Quality") +
  labs(title = "DISTRIBUTION OF RED WINE QUALITY")
`##############################################################################confusion matrix
t<-table(test$target,prednb)
tm<-as.data.frame(t)
tm
m1
m2## plotting confusion matrix
m3
m4
m5
ggplot(data = tm, aes(x=fixed_acidity, y=prednb, label="Freq"))


+ 
geom_tile(aes(fill=Freq)) + scale_fill_gradient(low="firebrick2", high="springgreen2") +
  theme_gray() + xlab("Actuel") + ylab("Predicted") + 
  geom_text(size=8) + ggtitle("Naive Byes")



###################################################################
results <- resamples(list(SVM=modelsvm, RF=modelrf, NB=modelnb, LR=modellr,KNN=fit.knn))
results
# summarize the distributions of the results 
pap<-summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)



####################################################################
installed.packages("glmnet")
0### data splitting
set.seed(1122)
tr.dat<-sample(nrow(wine), .7*nrow(wine),replace=FALSE)
Trainset<-wine[tr.dat,]
testset<-wine[-tr.dat,]
levels(Trainset$target)#(good or bad), levels is null
Trainset$target<-make.names(Trainset$target)
set.seed(534)
Trainset$target<-as.factor(Trainset$target)
###############################KNN###################


a<-read.csv('India.csv')
View(a)
dim(a)
India<-read.csv("India.csv", sep=",", stringsAsFactors=FALSE, header=TRUE)
View(India)
dim(India)
columns(India)
#Consider and import the Indian Anemia dataset using the command read.csv ("india.csv", sep=",",
                        # stringsAsFactors=FALSE, header=”TRUE”).
## such libraries
library(ggplot2)
library(lattice)
library(caret)
library(mlbench)
library(psych)
library(ggplot2)
#a) Divide the data into training and testing sets. Use 70% as the training set.
Indi <- sample(2, nrow(India), replace = T, prob = c(0.7, 0.3))
train <- India[Indi==1,]
test <- India[Indi==2,]
dim(train)
dim(test)
str(India)
unique(India)
nrow(India)
ncol(India)

INDIA<-sample(India(20000))
View(INDIA)

##library(caret)



#Find the most important variables that contribute most significantly to a response variable

#Selecting the most important predictor variables that explains the major part of variance of the response variable can be key to identify and build high performing models

# for random forest it can be effective to find a set of predictors that best
## explains the variance in the response variable 
library(randomForest)
library(varImp)
regressor <- randomForest(Target ~ . , datA=data, importance=TRUE)
# fit the random forest with default parameter
varImp(regressor) # get variable importance, based on mean decrease in accuracy
varImp(regressor, conditional=TRUE) # conditional=True, adjusts for 
#correlations between predictors
varimpAUC(regressor) # more robust towards class imbalance.



relevant<-train(anemia~.,data=)
#b) Using the India.csv data, (anemia2 being the target variable),
#fit the following models: 

# prepare training scheme for cross-validation
control <- trainControl(method="repeatedcv", number=10, repeats=5)
control
#SupportVector Machine (SVM)

library(kernlab)
set.seed(7)
modelSvm <- train(anemia2~., data=train, method="svmRadial", trControl=control)
modelSvm
predSvm=predict(modelSvm,newdata = test)
predSvm
confusionMatrix(predSvm,as.factor(test$anemia2))
#############################################

#Random Forest (RF)

set.seed(7)
modelrf <- train(anemia2~., data = train ,  method = "rf", trControl = control)
modelrf
predrf=predict(modelrf,newdata = test)
predrf
confusionMatrix(predrf,(test$anemia2))
####################################################

#K-Nearest Neighbors (KNN)
#metric = metric,

set.seed(7)
modelknn <- train(anemia2~., data = train ,  method = "knn", trControl = control)
modelknn
predknn=predict(modelknn,newdata = test)
predknn
confusionMatrix(predknn,(test$anemia2))
############################################

#Naïve Bayes (NB)

library(naivebayes)
modelnb <- train(anemia2~., data=train, method="naive_bayes", trControl=control)
modelnb
prednb=predict(modelnb,newdata = test)
prednb
confusionMatrix(prednb,(test$anemia2))
##################################################

#Logistic Regression (LR)

library(logistiregresion)
modellr <- train(anemia2~., data=train, method="logistic_regression", trControl=control)
modellr
predlr=predict(modellr,newdata = test)
predlr
confusionMatrix(predlr,(test$anemia2))
###########################################