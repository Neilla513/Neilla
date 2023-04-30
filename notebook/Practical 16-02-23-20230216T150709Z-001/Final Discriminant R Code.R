#load libraries
library(tidyverse)
library(caret)
library(MASS)
library(klaR)
library(mda)
library(sda)
library(ROCR)
library(pROC)
library(gplots)
theme_set(theme_classic())

##############################################################################
#############################DATA 1: BINARY CASE##############################
##############################################################################

#STEP 0:Read in the data into the R enviroment:
odata <- read_csv("C:/Users/OEA/Desktop/ADAGlobalConcept/Projects/odata.csv")

#View(odata)
head(odata)
dim(odata)

#STEP 1:Split the data into training and test set:
# Split the data into training (70%) and test set (30%)
set.seed(2503)
training.samples <- odata$overweight %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data <- odata[training.samples, ]
test.data <- odata[-training.samples, ]

#STEP 2: Normalize the data. Categorical variables are automatically ignored.
# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)
###########################################################

###########################################################
#Linear discriminant analysis - LDA
# Fit the model
set.seed(2023231)
model.lda <- lda(overweight~., data = train.transformed)
plot(model.lda)

# Make predictions
predictions.lda <- model.lda %>% predict(test.transformed)
names(predictions.lda)

#create ConfusionMatrix
table.lda <- table(predictions.lda$class,test.transformed$overweight)
confusionMatrix(table.lda, mode = "everything")

#ROC CURVE
roc.lda <- predict(model.lda, newdata = test.transformed, type = "prob")
alteredprob.lda <- roc.lda$class
labels <- test.transformed$overweight
pred.lda <- prediction(as.numeric(alteredprob.lda), as.numeric(labels))
pred.lda

perf.lda <- performance(pred.lda, "tpr", "fpr")
plot(perf.lda, avg = "threshold", col = 3,
     lwd = 4,
     #ylab = "Sensitivity",
     #xlab = "1 - Specificity",
     main = "ROC Curve comparing the performance of the 5 discriminant models")
abline(a=0, b=1)

#Area  under the curve (AUC)
auc.lda <- performance(pred.lda, "auc")
auc.lda <- unlist(slot(auc.lda, "y.values"))
auc.lda <- round(auc.lda, 4)
legend(.9, .2, auc.lda, title = "AUC-LDA", col = 3, pch = 3, lwd = 4)

###########################################################

###########################################################
#Quadratic discriminant analysis - QDA
# Fit the model
model.qda <- qda(overweight~., data = train.transformed)
#plot(model.qda)
# Make predictions
predictions.qda <- model.qda %>% predict(test.transformed)
names(predictions.qda)

#create ConfusionMatrix
table.qda <- table(predictions.qda$class,test.transformed$overweight)
confusionMatrix(table.qda, mode = "everything")

#ROC CURVE
roc.qda <- predict(model.qda, newdata = test.transformed, type = "prob")
alteredprob.qda <- roc.qda$class
labels <- test.transformed$overweight
pred.qda <- prediction(as.numeric(alteredprob.qda), as.numeric(labels))
pred.qda

perf.qda <- performance(pred.qda, "tpr", "fpr")
plot(perf.qda, avg = "threshold", col = 2, lwd = 4, add = TRUE)

#Area  under the curve (AUC)
auc.qda <- performance(pred.qda, "auc")
auc.qda <- unlist(slot(auc.qda, "y.values"))
auc.qda <- round(auc.qda, 4)
legend(.5, .2, auc.qda, title = "AUC-QDA", col = 2, pch = 3, lwd = 4)
###########################################################

###########################################################
#Regularized discriminant analysis - RDA
# Fit the model
model.rda <- rda(overweight~., data = train.transformed)
#plot(model.rda)
# Make predictions
predictions.rda <- model.rda %>% predict(test.transformed)
names(predictions.rda)

#create ConfusionMatrix
table.rda <- table(predictions.rda$class,test.transformed$overweight)
confusionMatrix(table.rda, mode = "everything")

#ROC CURVE
roc.rda <- predict(model.rda, newdata = test.transformed, type = "prob")
alteredprob.rda <- roc.rda$class
labels <- test.transformed$overweight
pred.rda <- prediction(as.numeric(alteredprob.rda), as.numeric(labels))
pred.rda

perf.rda <- performance(pred.rda, "tpr", "fpr")
plot(perf.rda, avg = "threshold", col = 4, lwd = 4, add = TRUE)

#Area  under the curve (AUC)
auc.rda <- performance(pred.rda, "auc")
auc.rda <- unlist(slot(auc.rda, "y.values"))
auc.rda <- round(auc.rda, 4)
legend(.7, .2, auc.rda, title = "AUC-RDA", col = 4, pch = 3, lwd = 4)
###########################################################

###########################################################
#Mixture discriminant analysis - MDA
# Fit the model
model.mda <- mda(overweight~., data = train.transformed)
#plot(model.mda)
# Make predictions
predictions.mda <- model.mda %>% predict(test.transformed)
names(predictions.mda)

#create ConfusionMatrix
table.mda <- table(predictions.mda,test.transformed$overweight)
confusionMatrix(table.mda, mode = "everything")

#ROC CURVE
labels <- test.transformed$overweight
pred.mda <- prediction(as.numeric(predictions.mda), as.numeric(labels))
pred.mda

perf.mda <- performance(pred.mda, "tpr", "fpr")
plot(perf.mda, avg = "threshold", col = 6, lwd = 4, add = TRUE)

#Area  under the curve (AUC)
auc.mda <- performance(pred.mda, "auc")
auc.mda <- unlist(slot(auc.mda, "y.values"))
auc.mda <- round(auc.mda, 4)
legend(.6, .2, auc.mda, title = "AUC-MDA", col = 6, pch = 3, lwd = 4)
###########################################################

###########################################################
#Flexible discriminant analysis - FDA
# Fit the model
model.fda <- fda(overweight~., data = train.transformed)
# Make predictions
predictions.fda <- model.fda %>% predict(test.transformed)
names(predictions.fda)

#create ConfusionMatrix
table.fda <- table(predictions.fda,test.transformed$overweight)
confusionMatrix(table.fda, mode = "everything")

#ROC CURVE
labels <- test.transformed$overweight
pred.fda <- prediction(as.numeric(predictions.fda), as.numeric(labels))
pred.fda

perf.fda <- performance(pred.fda, "tpr", "fpr")
plot(perf.fda, avg = "threshold", col = 5, lwd = 4, add = TRUE)

#Area  under the curve (AUC)
auc.fda <- performance(pred.fda, "auc")
auc.fda <- unlist(slot(auc.fda, "y.values"))
auc.fda <- round(auc.fda, 4)
legend(.8, .2, auc.fda, title = "AUC-FDA", col = 5, pch = 3, lwd = 4)
###########################################################

#Individual ROC CURVE
#LDA
plot(perf.lda, avg = "threshold", col = 3,
     lwd = 4, #ylab = "Sensitivity", #xlab = "1 - Specificity",
     main = "ROC Curve of Linear Discriminat Analysis")
abline(a=0, b=1)
legend(.8, .2, auc.lda, title = "AUC-LDA", col = 3, pch = 3, lwd = 4)

#QDA
plot(perf.qda, avg = "threshold", col = 2, lwd = 4, 
     main = "ROC Curve of Quadratic Discriminat Analysis")
abline(a=0, b=1)
legend(.8, .2, auc.qda, title = "AUC-QDA", col = 2, pch = 3, lwd = 4)

#RDA
plot(perf.rda, avg = "threshold", col = 4, lwd = 4, 
     main = "ROC Curve of Regularized Discriminat Analysis")
abline(a=0, b=1)
legend(.8, .2, auc.rda, title = "AUC-RDA", col = 4, pch = 3, lwd = 4)

#MDA
#ROC CURVE
plot(perf.mda, avg = "threshold", col = 6, lwd = 4, 
     main = "ROC Curve of Mixture Discriminat Analysis")
abline(a=0, b=1)
legend(.8, .2, auc.mda, title = "AUC-MDA", col = 6, pch = 3, lwd = 4)

#FDA
#ROC CURVE
plot(perf.fda, avg = "threshold", col = 5, lwd = 4, 
     main = "ROC Curve of Flexible Discriminat Analysis")
abline(a=0, b=1)
legend(.8, .2, auc.fda, title = "AUC-FDA", col = 5, pch = 3, lwd = 4)


###########################################################
#######################Metric Analysis#####################
models = c("LDA", "QDA", "RDA", "MDA", "FDA")

#############AUC ANALYSIS ###############
auc = c(0.5440, 0.5786, 0.5440, 0.5410, 0.5440)

plot(auc, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "AUC for Binary Case", col = 3, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

#############ACURRACY ANALYSIS ###############
acc = c(0.7662, 0.7463, 0.7662, 0.7463, 0.7662)

plot(acc, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Accuracy for Binary Case", col = 3, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

###############################################

############# BALANCED ACCURACY ANALYSIS ###############
bacc = c(0.5440, 0.5786, 0.5440, 0.5410, 0.5440)

plot(bacc, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Balanced Accuracy for Binary Case", col = 4, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

#############SENSITIVITY ANALYSIS ###############
sen = c(0.9130, 0.8571, 0.9130, 0.8820, 0.9130)

plot(sen, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Sensitivity for Binary Case", col = 2, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

###############################################

############# F1 Score ANALYSIS ###############
f1 = c(0.8622, 0.8440, 0.8622, 0.8478, 0.8622)

plot(f1, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "F1 Score for Binary Case", col = 5, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)


##############################################################################
###########################DATA 2: MULTICLASS CASE############################
##############################################################################

#STEP 0:Read in the data into the R enviroment:
bmicat <- read_csv("C:/Users/OEA/Desktop/ADAGlobalConcept/Projects/BMI-Data.csv")

#View(bmicat)
head(bmicat)
dim(bmicat)

#STEP 1:Split the data into training and test set:
# Split the data into training (70%) and test set (30%)
set.seed(2503280)
bmitraining.samples <- bmicat$BMI_Cat %>%
  createDataPartition(p = 0.7, list = FALSE)
bmitrain.data <- bmicat[bmitraining.samples, ]
bmitest.data <- bmicat[-bmitraining.samples, ]

#STEP 2: Normalize the data. Categorical variables are automatically ignored.
# Estimate preprocessing parameters
bmipreproc.param <- bmitrain.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
bmitrain.transformed <- bmipreproc.param %>% predict(bmitrain.data)
bmitest.transformed <- bmipreproc.param %>% predict(bmitest.data)
###########################################################

###########################################################
#Linear discriminant analysis - LDA
# Fit the model
set.seed(2503281)
model.bmilda <- lda(BMI_Cat~., data = bmitrain.transformed)
plot(model.bmilda)

# Make predictions
predictions.bmilda <- model.bmilda %>% predict(bmitest.transformed)
names(predictions.bmilda)

#create ConfusionMatrix
table.bmilda <- table(predictions.bmilda$class,bmitest.transformed$BMI_Cat)
confusionMatrix(table.bmilda, mode = "everything")
bmilda.Sensitivity <- (0.7857+0.7778+0.7143)/3 ### 0.7592667
bmilda.Specificity <- (0.8750+0.8000+0.9730)/3 ### 0.8826667
bmilda.BalancedAccuracy <- (0.8304+0.7889+0.8436)/3 ### 0.8209667
bmilda.F1 <- (0.8462+0.6087+0.7692)/3 ### 0.7413667


#Area  under the curve (AUC)
roc.bmilda <- predict(model.bmilda, newdata = bmitest.transformed, BMI_Cat = "prob")
alteredprob.bmilda <- roc.bmilda$class
bmilabels <- bmitest.transformed$BMI_Cat
pred.bmilda <- multiclass.roc(as.numeric(bmilabels), as.numeric(alteredprob.bmilda))
#Multi-class area under the curve for LDA: 0.8337

###########################################################
#Quadratic discriminant analysis - QDA
# Fit the model
set.seed(2503282)
model.bmiqda <- qda(BMI_Cat~., data = bmitrain.transformed)


# Make predictions
predictions.bmiqda <- model.bmiqda %>% predict(bmitest.transformed)
names(predictions.bmiqda)

#create ConfusionMatrix
table.bmiqda <- table(predictions.bmiqda$class,bmitest.transformed$BMI_Cat)
confusionMatrix(table.bmiqda, mode = "everything")
bmiqda.Sensitivity <- (0.9286+0.33333+0.14286)/3 ### 0.4682633
bmiqda.Specificity <- (0.5000+0.82857+1.00000)/3 ### 0.77619
bmiqda.BalancedAccuracy <- (0.7143+0.58095+0.57143)/3 ### 0.6222267
bmiqda.F1 <- ( 0.8387+0.33333+0.25000)/3 ### 0.47401

#Area  under the curve (AUC)
roc.bmiqda <- predict(model.bmiqda, newdata = bmitest.transformed, BMI_Cat = "prob")
alteredprob.bmiqda <- roc.bmiqda$class
bmilabels <- bmitest.transformed$BMI_Cat
pred.bmiqda <- multiclass.roc(as.numeric(bmilabels), as.numeric(alteredprob.bmiqda))
#Multi-class area under the curve for qda: 0.7239
###########################################################

###########################################################
#Regularized discriminant analysis - RDA
# Fit the model
set.seed(2503283)
model.bmirda <- rda(BMI_Cat~., data = bmitrain.transformed)
plot(model.bmirda)

# Make predictions
predictions.bmirda <- model.bmirda %>% predict(bmitest.transformed)
names(predictions.bmirda)

#create ConfusionMatrix
table.bmirda <- table(predictions.bmirda$class,bmitest.transformed$BMI_Cat)
confusionMatrix(table.bmirda, mode = "everything")
bmirda.Sensitivity <- (0.7857+0.7778+0.7143)/3 ### 0.7592667
bmirda.Specificity <- (0.8750+0.8000+0.9730)/3 ### 0.8826667
bmirda.BalancedAccuracy <- (0.8304+0.7889+0.8436)/3 ### 0.8209667
bmirda.F1 <- (0.8462+0.6087+0.7692)/3 ### 0.7413667


#Area  under the curve (AUC)
roc.bmirda <- predict(model.bmirda, newdata = bmitest.transformed, BMI_Cat = "prob")
alteredprob.bmirda <- roc.bmirda$class
bmilabels <- bmitest.transformed$BMI_Cat
pred.bmirda <- multiclass.roc(as.numeric(bmilabels), as.numeric(alteredprob.bmirda))
#Multi-class area under the curve for rda: 0.8337
###########################################################

###########################################################
#Mixture discriminant analysis - MDA
# Fit the model
set.seed(2503284)
model.bmimda <- mda(BMI_Cat~., data = bmitrain.transformed)
plot(model.bmimda)

# Make predictions
predictions.bmimda <- model.bmimda %>% predict(bmitest.transformed)


#create ConfusionMatrix
table.bmimda <- table(predictions.bmimda,bmitest.transformed$BMI_Cat)
confusionMatrix(table.bmimda, mode = "everything")
bmimda.Sensitivity <- (0.8571+0.6667+0.7143)/3 ### 0.7460333
bmimda.Specificity <- (0.8750+0.8571+0.9459)/3 ### 0.8926667
bmimda.BalancedAccuracy <- (0.8661+0.7619+0.8301)/3 ### 0.8193667
bmimda.F1 <- (0.8889+0.6000+0.7143)/3 ### 0.7344

#Area  under the curve (AUC)
roc.bmimda <- predict(model.bmimda, newdata = bmitest.transformed, BMI_Cat = "prob")
alteredprob.bmimda <- roc.bmimda
bmilabels <- bmitest.transformed$BMI_Cat
pred.bmimda <- multiclass.roc(as.numeric(bmilabels), as.numeric(alteredprob.bmimda))
#Multi-class area under the curve for mda: 0.8345
###########################################################

###########################################################
#Flexible discriminant analysis - FDA
# Fit the model
set.seed(2503285)
model.bmifda <- fda(BMI_Cat~., data = bmitrain.transformed)
plot(model.bmifda)

# Make predictions
predictions.bmifda <- model.bmifda %>% predict(bmitest.transformed)


#create ConfusionMatrix
table.bmifda <- table(predictions.bmifda,bmitest.transformed$BMI_Cat)
confusionMatrix(table.bmifda, mode = "everything")
bmifda.Sensitivity <- (0.7857+0.8889+0.7143)/3 ### 0.7963
bmifda.Specificity <- (0.9375+0.8000+0.9730)/3 ### 0.9035
bmifda.BalancedAccuracy <- (0.8616+0.8444+0.8436)/3 ### 0.8498667
bmifda.F1 <- (0.8627+0.6667+0.7692)/3 ### 0.7662

#Area  under the curve (AUC)
roc.bmifda <- predict(model.bmifda, newdata = bmitest.transformed, BMI_Cat = "prob")
alteredprob.bmifda <- roc.bmifda
bmilabels <- bmitest.transformed$BMI_Cat
pred.bmifda <- multiclass.roc(as.numeric(bmilabels), as.numeric(alteredprob.bmifda))
#Multi-class area under the curve for fda: 0.8469
###########################################################


###########################################################
#######################Metric Analysis#####################
models = c("LDA", "QDA", "RDA", "MDA", "FDA")

#############AUC ANALYSIS ###############
bmiauc = c(0.8337, 0.7239, 0.8337, 0.8345, 0.8469)

plot(bmiauc, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "AUC for Multiclass Case", col = 3, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

#############ACURRACY ANALYSIS ###############
bmiacc = c(0.7727, 0.6818, 0.7727, 0.7955, 0.7955)

plot(bmiacc, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Accuracy for Multiclass Case", col = 3, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

###############################################

############# BALANCED ACCURACY ANALYSIS ###############
bmibacc = c(0.8210, 0.6222, 0.8210, 0.8194, 0.8499)

plot(bmibacc, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Balanced Accuracy for Multiclass Case", col = 4, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

#############SENSITIVITY ANALYSIS ###############
bmisen = c(0.7593, 0.4683, 0.7593, 0.7460, 0.7963)

plot(bmisen, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "Sensitivity for Multiclass Case", col = 2, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)

###############################################

############# F1 Score ANALYSIS ###############
bmif1 = c(0.7414, 0.4740, 0.7414, 0.7344, 0.7662)

plot(bmif1, xaxt = "n", xlab = "Algorithms", type = "b", ylab = "F1 Score for Multiclass Case", col = 5, lwd = 5)
axis(side = 1, at = seq(1,5), labels = models)