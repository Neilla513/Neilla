########SUPERVISED MACHINE LEARNING WITH CARET PACKAGE##########################
#############################################################
###Compiled by Olawale Awe, PhD.
#############################################################################
#This Code will introduce you to using the caret #package for training machine learning models. The package caret is used for training both classification and regression models.
# We will consider a simple cases of classification and regression using inbuilt datasets. Feel free to adapt the codes to your datasets. We could not complete these tasts in class today because of time. 

####Load packages
library(caret)
library(mlbench)
library(psych)
library(MASS)
#####LOAD DATASET-PimaIndiansDiabetes####
data()###view various inbuilt data you can practice with in R
# load the inbuilt dataset PimaIndiansDiabetes
data(PimaIndiansDiabetes)
###Data Description
?PimaIndiansDiabetes
attach(PimaIndiansDiabetes)
pdata=PimaIndiansDiabetes
pdata #view the data
pdata
head(pdata)
# Check that no data is missing
apply(pdata,2,function(x) sum(is.na(x)))
?pdata
###################################################
######FEATURE SELECTION BEFORE CLASSIFICATION
### 1. Boruta algorithm to determine the best variables for the model. Include the important variables in the final model.
library(Boruta)
borC = Boruta(factor(diabetes)~., data = pdata, doTrace = 2, maxRuns=25)
print(borC)
par(pty='m')
plot(borC,las=2,cex.axis=0.7)
plotImpHistory(borC)
bor1=TentativeRoughFix(borC)
attStats(bor1)
###############################################
#####################PARTITION THE DATA########################################
ind <- sample(2, nrow(pdata), replace = T, prob = c(0.7, 0.3))
train <- pdata[ind==1,]
test <- pdata[ind==2,]
# prepare training scheme for cross-validation
control <- trainControl(method="repeatedcv", number=10, repeats=5, classProbs = TRUE)
# train the LVQ model
set.seed(7)
modelLvq <- train(factor(diabetes)~., data=train, method="lvq", trControl=control)
modelLvq
predLVq=predict(modelLvq,newdata = test)
predLVq
cm=confusionMatrix(predLVq,as.factor(test$diabetes), mode='everything')
cm
########Plot The Confusion matrix. You can do same for other models
cm$table
fourfoldplot(cm$table, color = c("red", "#6699CC"))
####Perform inference to determing most important variaables used for classification
varImp(modelLvq, scale=T)
####Plot your result
plot(varImp(modelLvq, scale=T))
###The graph shows that glucose level is the most important feature for classifying diabetic patients. 
#You can try this for other models

# train the GBM model
set.seed(7)
modelGbm <- train(diabetes~., data=train, method="gbm", trControl=control, verbose=FALSE)
modelGbm
predGbm=predict(modelGbm,newdata = test)
predGbm
cmg=confusionMatrix(predGbm,as.factor(test$diabetes))
cmg
cmg$table
fourfoldplot(cmg$table, color = c("red", "blue"))
# train the SVM model
set.seed(7)
modelSvm <- train(diabetes~., data=train, method="svmRadial", trControl=control, preProc=('scale'),tuneLength=5)
modelSvm
predSvm=predict(modelSvm,newdata = test)
predSvm
confusionMatrix(predSvm,as.factor(test$diabetes))
######################################################################

####Naive Bayes
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
##########################################################################################################
####Neural Network
modelnn <- train(factor(diabetes)~., data=train, method="nnet", trControl=control,hidden=c(5,3),linear.output=T)
modelnn
plot(modelnn)
prednn=predict(modelnn,newdata = test)
prednn
confusionMatrix(prednn,as.factor(test$diabetes))
######################################################################
#####Random Forest
set.seed(7)
model.rf <- train(factor(diabetes)~.,
                 data = train, 
                 method = "rf",
                 metric = 'ROC',
                 trControl = control)
model.rf
predrf=predict(model.rf,newdata = test)
predrf
confusionMatrix(predrf,as.factor(test$diabetes))
##################################################
#############KNN MODEL
set.seed(7)
fit.knn <- train(factor(diabetes)~.,
                 data = train , 
                 method = "knn",
                 metric = 'ROC',
                 trControl = control, tuneLength=20)
fit.knn
predknn=predict(fit.knn,newdata = test)
predknn
confusionMatrix(predknn,as.factor(test$diabetes))


##############################################
#####GLM
set.seed(7)
modelglm <- train(factor(diabetes)~.,
                  data = train, 
                  method = "glm",
                  #metric = metric,
                  trControl = control)
modelglm
predglm=predict(modelglm,newdata = test)
predglm
confusionMatrix(predglm,as.factor(test$diabetes))
###################################################
##############################################
##################LDA###################
set.seed(7)
model.lda <- train(factor(diabetes)~.,
                  data = train, 
                  method = "lda",
                  #metric = metric,
                  trControl = control)
model.lda
predglm=predict(model.lda,newdata = test)
predglm
confusionMatrix(predglm,as.factor(test$diabetes))


###############################################
# collect all resamples and compare MODELS
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, NB=modelnb,NN=modelnn,RF=model.rf, LDA=model.lda, SVM=modelSvm,GLM=modelglm, DT=modelDT,KNN=fit.knn))
# summarize the distributions of the results 
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
#####################################################################
#########################################################################
####Now repeat the entire process using your own data, choosing the target variable of your choice. Tabulate your results and compare with other models like LR, LM, LDA, LVQ for binary cases. Also compute ROC and AUC for these models. Tune your models to get the best hyperparameters. 

#########################################################################REGRESSION CASE ########################################
######################Regression###############
library(caret)
library(glmnet)
library(mlbench)
library(psych)
library(RSNNS)
library(generics)
library(Rcpp)
##################KNN WITH CONTINUOS RESPONSE###########################################

###Let us practice with another inbuilt data-BostonHousing
?BostonHousing
data("BostonHousing")
data <- (BostonHousing)
str(data)
bdata=data
bdata
###################################################
####CORRELATION
library(corrplot)
bdata
k=cor(bdata)
corrplot(k)
corrplot(k, type='upper',method='ellipse')
corrplot(k, type='upper',method='pie')
####Debug where necessary. Dont be afraid of making mistakes!
#####Data Partition
set.seed(1234)
ind <- sample(2, nrow(bdata), replace = T, prob = c(0.75, 0.25))
train_r <- bdata[ind == 1,]
test_r <- bdata[ind == 2,]

#########Train Control######
trControl <- trainControl(method = 'repeatedcv', 
                          number = 10,   
                          repeats = 10) 
##Free free to try different numbers
#################################################
########Fit the Regression Model################
fitreg <- train(medv ~.,
             data = train_r,
             tuneGrid = expand.grid(k=1:70),
             method = 'knn',
             metric = 'Rsquared',
             trControl = trControl,
             preProc = c('center', 'scale'))
fitreg
####################################################
k=varImp(fit)  ####Check Variable Importance
plot(fit)
plot(k)
########Prediction and Model Performance
pred <- predict(fit, newdata = test_r)
RMSE(pred, test_r$medv)
plot(pred ~ test_r$medv)
##############################################
#######
#############################################
###Note that in Classification, we use confusion matrix, while in regression we dont. Please repeat same for other regression models you wish to fit.
################################################################ Many ML models can accommodate both classification and regression. 
##Other regression models you can try are found below. Dont forgest to predict and validate with the test data
###############################################################################
######Decision Tree
set.seed(7)
modelDT <- train(medv~., data = train_r, method = "rpart",
                 #metric = metric,
                 trControl = control)
modelDT
plot(modelDT$finalModel, uniform=TRUE,
     main="Classification Tree")
text(modelDT$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
suppressMessages(library(rattle))

fancyRpartPlot(modelDT$finalModel)

library(rpart.plot)

rpart.plot(modelDT$finalModel)
plot(varImp(modelDT, scale=T))
plot(modelDT)

#######Bayesian Ridge Regression
set.seed(7)
modelbridge<- train(medv~., data=train_r, method="bridge", trControl=control)

modelbridge
plot(varImp(modelbridge, scale=T))
#############################################################################
###Elastic Net
#############################################################################
set.seed(7)
modelenet <- train(formula2, data=train_r, method="enet", trControl=control)
modelenet
plot(varImp(modelenet, scale=T))
############################################################################
###########Ridge Regression
set.seed(7)
modelridge <- train(medv~., data=train_r, method="ridge", trControl=control)
modelridge
plot(varImp(modelridge, scale=T))
##########################################################################
###Random Forest
############################################################################
set.seed(7)
modelrf <- train(medv~., data=train_r, method="rf", trControl=control)
modelrf$results
modelrf
plot(varImp(modelrf, scale=T))
############################################################################
####################Bayesian LASSO###########################################
set.seed(7)
modelblasso <- train(medv~., data=train_r, method="blasso", trControl=control)
modelblasso
plot(varImp(modelblasso, scale=T))
############################################################################
###LASSO
set.seed(7)
modellasso <- train(medv~., data=train_r, method="lasso", trControl=control)
plot(modellasso)
plot(varImp(modellasso, scale=T))

################################################
modelgam <- train(medv~., data=train_r, method="gbm", trControl=control, verbose=FALSE)
modelgam$finalModel
modelgam
plot(varImp(modelgam, scale=T))

############################################################################
# collect resamples and list all the models here. 
#results <- resamples(list(Lasso=modellasso, Bridge#=modelbridge, Enet=modelenet, RLM=modelrlm,Blasso=modelblasso,RF=modelrf,Ridge=modelridge, SVM=modelsvm, CART=fit.DT,KNN=modelknn))

# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

####Let me know which model is not running
################################################################################






####UNSUPERVISED MACHINE LEARNING CLUSTERING EXAMPLE######################################
gdp = read.csv("realGP.csv",header=T)
gdp
dim(gdp)
gdp = gdp[ ,-c(1)]
gdp
dim(gdp)
describe(gdp)
#####Clustering###################
# Loading the data set
df <- scale(gdp) # Scaling the data

dft <- as.data.frame(t(as.matrix(df)))
dft
kmeans(dft, 3, iter.max = 10, nstart = 1)

#install.packages("factoextra")
library(factoextra)
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(dft, 3, nstart = 25)

km.res$cluster

head(km.res$cluster, 3)

km.res$centers

fviz_cluster(km.res, dft,
             palette = "Set2", ggtheme = theme_minimal())

# Show text only
fviz_cluster(km.res, dft, geom = "text")

# PAM clustering
# ++++++++++++++++++++
require(cluster)
pam.res <- pam(dft, 3)
# Visualize pam clustering
fviz_cluster(pam.res, geom = "text", ellipse.type = "norm")

# Hierarchical clustering
# ++++++++++++++++++++++++
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(dft, k = 3, hc_method = "complete", cex=.7)
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = T, rect = TRUE, cex=.8)###Dendrogram
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")

###############################################################################################################################################################


###PREPARE AND SUBMIT YOUR POWERPOINTS 2HOURS BEFORE YOUR PRESENTATION. BEST WISHES! OOA.