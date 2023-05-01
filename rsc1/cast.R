rm(list=ls())
setwd("set working directory")
View(CAST1cc.csv)
data<-read.csv("CAST1cc.csv")
View(data)
summary((data))
library(xtable)
xtable(data)
anova(data)
##Using appropriate numerical,
#tabular or graphical summaries, check and then describe the distribution of
#each variable.
Height<-data$height
Weight<-data$weight
Sex<-data$sex
Age<-data$age
xtable(summary(data))
par(mfrow=c(2,2))
hist(Height)
hist(Weight)
hist(Sex)
hist(Age)
plot(hist(Weight)  )        
plot(density(Height))
plot(density(Sex))
plot(density(Age))
boxplot(Height)
boxplot(Weight)
boxplot(Sex)
boxplot(Age)
a<-str(data)
a
xtable(a)
dim(data)
qqnorm(Sex, main=' Q-Q Plot',ylab='height')
#qqline(Sex,col='blue')
anova(data)

anova(data)
fit<-lm(Weight ~ Sex+Height+Age)
fit
summary.aov(fit)
summary(fit)
a<-anova(fit)
a
xtable(a)




#Getting the predicted or fitted values
fitted(fit)
#residuals
resids<-residuals(fit)
resids
#confidence interval for each output
pred=predict.lm(fit, interval="confidence")
pred

# male weight
wt_male<-predict(fit, newdata=data.frame( height=c(163), sex=c('male'), age=c(67) ) )
wt_male
#female weight
wt_female<-predict(fit, newdata=data.frame(
  height=c(163), sex=c('female'), age=c(67)))
wt_female
# Checking Whether I am overweight or underweight
# body mass index for me.
height_m<-1.63
wt_m<-55
bmI_me<- wt_m/(height_m)^2
bmI_me
