rm(list=ls())
setwd("~/Documents/AIMS2223/REVIEW COURSES/BLOCK4/ML/notebook")
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
Tribe
#3
Income<-sample(150000:200000, 50, replace=T)
Income
#4
Weight<-sample(c(1,2),50,replace=T)
Weight
#5
Grade<-sample(c(1,2), 50, replace=T)
Grade
#6 we need  to convert into factor the categorical 
Gender<-sample(c(1,2), 50, replace=T)
Gender
#7
EduLevel<-sample(c(1,2,3), 50, replace=T)
EduLevel
#8
MStatus<-sample(c(1,2), 50, replace=T)
MStatus
#9
HUnit<-sample(c(1,2,3,4), 50, replace=T)
HUnit
#10 any continuous variable
Children<-sample(0:10, 50, replace=T)
Children
Mdata=data.frame(Age, Tribe, Income, Weight, Grade, Gender, EduLevel, MStatus, HUnit, Children)
View(Mdata)
