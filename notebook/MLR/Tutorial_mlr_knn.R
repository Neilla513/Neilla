#1. Install packages
install.packages("caret")

## If you are having problems with packages, you can install the caret packages and all packages that you might need by typing
install.packages("caret", dependencies=c("Depends", "Suggests"))

##Loading caret

##The caret package provides a consistent interface into hundreds of machine learning algorithms and provides useful convenience methods for data visualization, 
#data resampling, model tuning and model comparison, among other features. It’s a must have tool for machine learning projects in R.
library(caret)

##Load the Data
## For this exercise, we will use the iris flowers dataset, which is well-known in machine learning and statistics by almost everyone.
#The dataset contains 150 iris flower observations.
#There are four columns of flower measurements in centimeters.
#The fifth column contains the flower species observed. All of the flowers observed are from one of three species.


# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris
View(dataset)

##Create a Validation Dataset
##We will divide the loaded dataset in half, using 80% to train our models and the remaining 20% as a validation dataset.


# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
training <- dataset[validation_index,]
dim(validation)
dim(training)

##You now have training data in the training variable and a validation set we will use later in the validation variable.
##3. Summarize Dataset
# dimensions of the training set
dim(training)

#type of attributs
# list types for each attribute
sapply(training, class)

names(training) #columns names
str(training) #tructure
anyNA(training) #check missing values
## take a look at the first 5 rows of the data
head(dataset)

# list the levels for the class
levels(training$Species)

##let check the Class Distribution

# summarize the class distribution
percentage <- prop.table(table(training$Species)) * 100
cbind(freq=table(training$Species), percentage=percentage)

## summarize attribute distributions
summary(training)



# split input and output
x <- training[,1:4]
y <- training[,5]        

##SOME Vizualisation
# barplot for class breakdown
plot(y)

##let’s look at scatterplots of all pairs of attributes and color the points by class
# scatterplot matrix
#featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
#This is useful to see  clearly the different distributions of the attributes for each class value.


featurePlot(x=x, y=y, plot="box")

#Use boxplots to see the difference in distribution of each attribute by class value. 
# This help to see the Gaussian-like distribution (bell curve) of each attribute.
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#Set-up the test harness to use 5-fold cross validation.
#This will split our training dataset into 5 parts, train in 4 and test on 1 and release for all combinations of train-test splits.
## To evaluate models, we use the "Accuracy" metric. This is a percentage calculated by dividing the number of correctly predicted instances by the total number of instances in the dataset and multiplying the result by 100.

# Run algorithms using 5-fold cross validation

control <- trainControl(method="cv", number=5)
metric <- "Accuracy"

##4. Build ML Models
##For this exercise, we are going to use : 
##k-Nearest Neighbors (kNN).
##Support Vector Machines (SVM) with a linear kernel.
##Random Forest (RF)

##We reset the random number seed before reach run to ensure that the evaluation of each algorithm is performed using exactly the same data splits.
## It ensures the results are directly comparable.

# kNN
set.seed(7)
fit.knn <- train(Species~., data=training, method="knn", metric=metric, trControl=control)

# SVM
#set.seed(7)
#fit.svm <- train(Species~., data=training, method="svmRadial", metric=metric, trControl=control)
# Random Forest
#set.seed(7)
#fit.rf <- train(Species~.,data=training, method="rf", metric=metric, trControl=control)


##Select Best Model

# summarize accuracy of models
#results <- resamples(list(knn=fit.knn rf=fit.rf))
#results <- resamples(list(knn=fit.knn))
#summary(results)

print(fit.knn)

## Make prediction

# prediction of KNN on the validation dataset

predictions <- predict(fit.knn, validation)
confusionMatrix(predictions, validation$Species)


