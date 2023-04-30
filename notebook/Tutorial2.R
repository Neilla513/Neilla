#install.packages("caret,dependencies=c("Depends", "suggest"))

data(iris)
dataset<-iris
View(dataset)
# iris is a floor, depends on the length for instance they can classify

## Now we split the data
data1=read.csv('indmover.csv', sep=",", stringsAsFactors = FALSE, header = TRUE)
Ind=sort(sample(nrow(dataset), nrow(dataset)*0.80))
library(caret)

validation_index<-createDataPartition(dataset$Species, p=0.80, list=FALSE)

training<-dataset[validation_index,]
summary(training)
dim(training)
sapply(training, class)
x<-training[,1:4]
y<-training[,5]
plot(y)#
plot(x)
scale<-
featurePlot(x=x, y=y, plot="density", scales=scales)
control<-trainControl(method="cv",number=5)
metric<-"Accuracy"
##4. Build ML Models, we are going to use in this case:KNN(compute the distance 
##with all the points and select the may 5 close one and then i can say knn is this class of points), 
#SVM with a linear kernel, Random Forest
##KNN
set.seed(7)## set the random generator if not the values will change randomly every time
fit.knn<-train(Species~., data=training, method="knn", metric=metric, trControl=control)
 print(fit.knn)
## prediction of KNN on teh validation dataset
 predictions<-predict(fit.knn, validation)
confusionMatrix(predictions, validation$Species)
 # SVM