rm(list=ls())
a<-setwd("~/Documents/AIMS2223/REVIEW COURSES/BLOCK4/ML/notebook")
df<-read.csv('indmover.csv')
df


## TASK2: We create a 50 by 10 hypothetical dataframe with the following variables,
## That means 50 observations and 10 variables. The variables are create randomly  so the don't have actuel
##  relationship
# 1
Age<-sample(30:60, 50, replace=T)
Age
#2
Tribe<-sample(c(1,2,3),50, replace=T)
Tribe<-as.factor(Tribe)
#3
Income<-sample(150000:200000, 50, replace=T)
Income
#4
Weight<-sample(c(1,2),50,replace=T)
Weight<-as.factor(Weight)
#5
Grade<-sample(c(1,2), 50, replace=T)
Grade<-as.factor(Grade)
#6 we need  to convert into factor the categorical 
Gender<-sample(c(1,2), 50, replace=T)
Gender<-as.factor(Gender)
#7
EduLevel<-sample(c(1,2,3), 50, replace=T)
EduLevel<-as.factor(EduLevel)
#8
MStatus<-sample(c(1,2), 50, replace=T)
MStatus<-as.factor(MStatus)
#9
HUnit<-sample(c(1,2,3,4), 50, replace=T)
HUnit<-as.factor(HUnit)
#10 any continuous variable
Children<-sample(0:10, 50, replace=T)
Children
Mdata=data.frame(Age, Tribe, Income, Weight, Grade, Gender, EduLevel, MStatus, HUnit, Children)
View(Mdata)
## a) we are now explllore the data using the summary command
summary(Mdata)

## b) We export the dataframe into the working directory and rename it as "Practdata.csv"
write.csv(Mdata,'Practdata.csv', row.names=F)
# c) we import back into R using read.csv command
read.csv('Practdata.csv')
## d) Let's perform another task on the data
set.seed(123)
## note about the relevance
library(Boruta)
boruta<-Boruta(Income~., data=Mdata, doTrace=100)
plot(boruta)
Boruta(EduLevel, Mdata)
Bor


## TASK3
library(dplyr)
##Dtataframe with Edulevel>2
## take all the rows with age greater than 30
filter(Mdata, Age>30)
##a) dataframe with Edulevel>2
dt1=Mdata %>% filter(as.numeric(EduLevel>2))
df1=Mdata %>% filter(EduLevel>2)
df1
#b)dataframe with Age >25 and HUnit<=3
df2=Mdata%>%filter(Age>25 , HUnit<=3)
df2
##c)Dtaframe with Tribe =>3 and Grade=2
df3=Mdata%>%filter(Tribe>=3,Grade==2)
df3
Mdata %>% filter(Age>30)
Mdata %>% select(Tribe)

##d) Now Subset more dataframe of our choice 
dim(df3)
View(df3)
View(df2)
View((df1))
dim(df1)
## e) We examine their dimension
## f) subset all continuous variables in Mdata into a separate dataframe
#dataframe and name it cdat.
cdat=Mdata%>%select(Age,Income, Children)
cdat
d=Mdata%>%select(where(is.numeric))
e=Mdata%>%select_if(is.numeric)
library(corrplot)
dim(e)
dim(d)
dim(cdat)
##f)
rcdat=cor(cdat)
par(mfrow=c(1,3))
re=cor(e)
rd=cor(d)
corrplot(re, method = 'color', order='alphabet')
# correlation plot
corrplot(rcdat, method = 'color', order='alphabet')
## h) Let's describe cdat
corrplot(rd)
library(psych)
describe(cdat)
## NB

##TASK4 consider and import the indmover.csv dataset using the command
## read.csv

Indmover<-read.csv('indmover.csv', sep=",")#, stringsAsFactors = FALSE, header = "TRUE")
View(Indmover)
dim(Indmover)
library(Boruta)
boruta=Boruta(overwgt~.,data=Mdata, doTrace=100)

## we divide the data on to training and testing set:
## 70% training and 30% testing set
data1=read.csv('indmover.csv', sep=",", stringsAsFactors = FALSE, header = TRUE)
Ind=sort(sample(nrow(data1), nrow(data1)*0.70))

dim(data1)

trainD=Ind[Ind]
testD=Ind[-Ind]
## We have labelled the train data as trainD and test data as testD

## Now we examine the dimension of each one
dim(trainD)
dim(testD)
## We use the boruta overwgt as the target variable 
library(Boruta)
boruta=Boruta(overwgt~.,data=data1, doTrace=100)
## we plot and save our results
plot(boruta)