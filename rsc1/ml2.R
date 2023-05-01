rm(list=ls())
#setwd("~/Documents/AIMS2223/REVIEW COURSES/BLOCK4/ML/Assignments/Assign2")
## packages loading
library(psych)
library(ggplot2)
library(caret)
library(mice)
library(lavaan)
library(Boruta)
library(pROC)
library(xgboost)
library(semPlot)
library(xtable)
install.packages("psych")
##################################################################3
cfa<-read.csv('cfa3.csv')
head(cfa)
str(cfa)
sum(is.na(cfa))
colSums(is.na(cfa))
dim(cfa)
attach(cfa)
###########################################################################
#imputation
ip=5
mice_nu<-mice(cfa, m=ip,method='pmm', maxit=20)
cf<-complete(mice_nu,3)
#############################################################
##Determine whether the data  are suitable  for factor analysis 
#kaizer mayer oklin test (KMO)
kmo<-KMO(cf)
kmo
#####Bartletts test
par(mfrow=c(1,1))
R_sqrt<-cor(cf)
corrplot::corrplot(R_sqrt, method='number', type='full')
bart<-cortest.bartlett(R_sqrt)
bart
#############################################################################
infa<-fa.parallel(cf)
#scree plot
scree(cf)
#################################################################
########### Extract initial factors
fac<-fa(cf, fm='minres',nfactors=3, rotate='varimax', digits=3)
print(fac$loadings, cut = 0.6)
fa.diagram(fac$loadings, cut = 0.6, rsize =0.7)
fac
####################################################
modcfa<-'MR1 = ~Item1 + Item2 + Item3+Item6+Item7+Item13+Item14+Item15+Item16+Item17+
Item18+Item19+Item20+Item23+Item24+Item25+Item26+Item27+Item28+Item29
+Item30+Item31+Item32+Item33
MR2 = ~Item34+Item35+Item36+Item37+Item38+Item40
MR3= ~Item5+Item8+Item9+Item10+Item11+Item12'
library(xtable)

fitcf<-cfa(modcfa, data=cf)
fitcf
summary(fitcf, standardized = T)
semPlot::semPaths(fitcf, "std")
###############################################################
################################################################
## EXERCISE 2##################
setwd("~/Documents/AIMS2223/REVIEW COURSES/BLOCK4/ML/Assignments/Assign2")
lung<-read.csv('Lung.csv', header =T)
View(lung)
sum(is.na(lung))
str(lung)
attach(lung)
names(lung)
kmo

## Now we transform as factor our features except age
## because of numerical or a big number of entries
#f_var<- c('GENDER', 'SMOKING', 'YELLOW_FINGERS', 'ANXIETY',
         #   'PEER_PRESSURE', 'CHRONIC.DISEASE', 'FATIGUE', 'ALLERGY', 
        # 'WHEEZING', 'ALCOHOL.CONSUMING', 'COUCHING', 'SHORTNESS.OF.BREATH',
        # 'SWALLOWING.DIFFICULTY', 'CHEST.PAIN', 'LUNG_CANCER')
#lung[f_var]<-lapply(lung[f_var], factor)

lung[,-c(2)]<-lapply(lung[,-c(2)], factor)

str(lung)
 attach(lung)
#colSums(is.na(lung))
##############################################
### features selections
set.seed(7)
bor<-Boruta(LUNG_CANCER~., data=lung, doTrace=3)        
bor

#Boruta performed 67 iterations in 8.117009 secs.
##13 attributes confirmed important: ALCOHOL.CONSUMING, ALLERGY, ANXIETY, CHEST.PAIN,
#CHRONIC.DISEASE and 8 more;
#2 attributes confirmed unimportant: AGE, SMOKING;
plot(bor)
getSelectedAttributes(bor,withTentative = T)
#[1] "GENDER"                "YELLOW_FINGERS"        "ANXIETY"              
#[4] "PEER_PRESSURE"         "CHRONIC.DISEASE"       "FATIGUE"              
#[7] "ALLERGY"               "WHEEZING"              "ALCOHOL.CONSUMING"    
#[10] "COUGHING"              "SHORTNESS.OF.BREATH"   "SWALLOWING.DIFFICULTY"
#[13] "CHEST.PAIN" 
## We now see that the age and feature smoking are not identify here they are corresponding of 2 and 3 respectively on dataset
lune<-lung[,-c(2,3)]
###################################################################################
## Now we split the data
set.seed(7)
LUNE <- sample(2, nrow(lune), replace = T, prob = c(0.75, 0.25))
train <- lune[LUNE==1,]
test <- lune[LUNE==2,]
dim(train)
dim(test)#225  14 dim(test) [1] 84 14
str(lune)
unique(lune)
names(lune)
################################################################################################
train$LUNG_CANCER<-make.names(train$LUNG_CANCER)
set.seed(16)
train$LUNG_CANCER<-as.factor(train$LUNG_CANCER)
###########################################################################
############let's check the proportions of the splitted data
## the split proportion is 70/30 percent
set.seed(16)
prop.table(table(lune$LUNG_CANCER))
#      NO       YES 
#0.1262136 0.8737864 
prop.table(table(train$LUNG_CANCER))
#       NO       YES 
#0.1155556 0.8844444 
prop.table(table(test$LUNG_CANCER))
#       NO       YES 
#0.1547619 0.8452381 
View(lune)
dim(lune)#309  14
## tuning parameter
control<-trainControl(method = "repeatedcv", number = 10, repeats=5)
control

###########################################
set.seed(50)
library(kernlab)
modelsvm<-train(target~., data = train, method ="svmRadial", trControl=control)
modelsvm
predsvm=predict(modelsvm,newdata = test)
predsvm
a<-confusionMatrix(predsvm,as.factor(test$target))
m1<-a$byClass[c(1,2,5,7,11)]  
m1
#####################################Bagging####
library(ipred)
#bagging<-bagging(formula=LUNG_CANCER~., data = train) 
set.seed(15)
modelb<-train(LUNG_CANCER~., data=train, method="treebag", trControl=control)
modelb
predb=predict(modelb, newdata=test)
predb
a<-confusionMatrix(predb,test$LUNG_CANCER)
m1<-a$byClass[c(1,2,5,7,11)]
m1
xtable(m1)
library(stargazer)
stargazer(m1)
#####GB Boosting ##########
library(gbm)
set.seed(15)
modelgb<-train(LUNG_CANCER~., data = train, method = "gbm", trControl = control)
modelgb
predgb=predict(modelgb, newdata = test)
predgb
b<-confusionMatrix(predgb, test$LUNG_CANCER)
m2<-b$byClass[c(1,2,5,7,11)]
m2
########LVQ#########
set.seed(15)
modelvq<-train(LUNG_CANCER~., data = train, method = "lvq", trControl = control, positive ='YES')
modelvq
predvq=predict(modelvq, newdata = test)
predvq
c<-confusionMatrix(predvq, test$LUNG_CANCER)
m3<-c$byClass[c(1,2,5,7,11)]
m3
##############NN#################
set.seed(15)
modelnn<-train(LUNG_CANCER~., data = train, method = "nnet", trControl = control)
modelnn
prednn=predict(modelnn, newdata = test)
prednn
d<-confusionMatrix(prednn, test$LUNG_CANCER)
m4<-d$byClass[c(1,2,5,7,11)]
m4
############LDA########
set.seed(15)
modelda<-train(LUNG_CANCER~., data = train, method = "lda", trControl = control)
modelda
predda=predict(modelda, newdata = test)
predda
plot(predda)
e<-confusionMatrix(predda, test$LUNG_CANCER)
m5<-e$byClass[c(1,2,5,7,11)]
m5
stargazer(m5)
e
d
c
m1
set.seed(15)
accuracy<-round(data.frame(Bagging=a$overall[1], GB=b$overall[1],
                           LVQ=c$overall[1], NN=d$overall[1],LDA=e$overall[1]),3)
accuracy
bg<-predict(modelb, test,type="prob")
bg_roc<-roc(predictor=bg$YES, response=test$LUNG_CANCER, levels=levels(test$LUNG_CANCER))
au1<-bg_roc$auc
au1
#################
gb<-predict(modelgb, test,type="prob")
gb_roc<-roc(predictor=gb$YES, response=test$LUNG_CANCER, levels=levels(test$LUNG_CANCER))
au2<-gb_roc$auc
au2
######################
lvq<-predict(modelvq, test,type="prob")
lvq_roc<-roc(predictor=lvq$YES, response=test$LUNG_CANCER, levels=levels(test$LUNG_CANCER))
au3<-lvq_roc$auc
au3
##############
nn<-predict(modelnn, test,type="prob")
nn_roc<-roc(predictor=nn$YES, response=test$LUNG_CANCER, levels=levels(test$LUNG_CANCER))
au4<-nn_roc$auc
au4
############LDA$###########
lda<-predict(modelda, test,type="prob")
lda_roc<-roc(predictor=lda$YES, response=test$LUNG_CANCER, levels=levels(test$LUNG_CANCER))
au5<-lda_roc$auc
au5
par(mfrow=c(3,2))
plot(lda_roc)
plot(nn_roc)
plot(gb_roc)
plot(lvq_roc)
plot(bg_roc)

stargazer(m2)
ggplot(lune, aes(x=LUNG_CANCER, fill=LUNG_CANCER)) + geom_bar(stat = "count") +
  geom_text(position = "stack", stat='count', aes(label=..count..), vjust=-0.5) +
  labs(y="Observation", x="Cancer") +
  labs(title = "DISTRIBUTION OF LUNG_CANCER")
plot(predvq, col= rainbow(2))
library(corrplot)
library(semPlot)
install.packages('stargarzer', dependencies = T)
