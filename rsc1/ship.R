#setwd("C:/Users/julia/OneDrive - Universita degli Studi Roma Tre/Cameroon/AIMS 2022/Slides/Rcode")
## From McCullagh and Nelder, p136
library(MASS)
data(ships)
attach(ships)
# Data frame giving the number of damage incidents and aggregate months of service
#by ship type, year of construction, and period of operation.
# type: "A" to "E".

# year: of construction: 1960–64, 65–69, 70–74, 75–79 
#                (coded as "60", "65", "70", "75").
# period. of operation : 1960–74, 75–79.
# service: aggregate months of service.
# incidents: number of damage incidents.
# 
# Source :  P. McCullagh and J. A. Nelder, (1983), Generalized Linear Models. 
#Chapman & Hall, section 6.3.2, page 137
ships2 <- subset(ships, service > 0)
ships2$year <- as.factor(ships2$year)
ships2$period <- as.factor(ships2$period)
     detach(ships) ; attach(ships2)  
head(ships2)
str(ships2)
summary(ships2)
X11()#plot on new page
par(mfrow = c(2,2))
plot(incidents/service ~ type + year + period)
       yr <- as.factor(year) ;par(mfrow = c(2,2))
plot(1000*incidents/service ~ type + year + period)

par(mfrow = c(1,1))#delimite the array of the figure
plot(xtabs(incidents ~ type + period))
rate<-1000*incidents/service
plot(xtabs(rate ~ type + period))

## using one covariate: period
glm0 <- glm(incidents ~ period, offset = log(service),
            family = poisson(link = "log"), data = ships2)
summary(glm0)
anova(glm0)
##classify two periods 
Period<-factor(period, labels=c("60-74","75-79"))
table(Period, period) 

glmP <- glm(incidents ~ Period, offset = log(service),
            family = poisson(link = "log"), data = ships2)
summary(glmP)
anova(glmP)

glm1 <- glm(incidents ~ type + year + period, offset = log(service),
            family = poisson(link = "log"), data = ships2)
summary(glm1)

anova(glm1)
par(mfrow = c(2,2))
plot(glm1)

###test for interactions
glm3 <- glm(incidents ~ type * period, offset = log(service),
            family = poisson(link = "log"), data = ships2)
summary(glm3)
anova(glm3)
##Are there significant interaction terms?

      # ratepm <-round(1000*ships2$incidents/ships2$service,1)
      # cbind(ships2,ratepm)  
#reproduce table in M&N
 xtabs(incidents ~ type + year)/xtabs(service ~ type + year)*1000
  round(xtabs(incidents ~ type + year)/xtabs(service ~ type + year)*1000,2)
          # The second is easier to read. and ..
  table(type,year); round(xtabs(service/1000 ~ type + year),1) # .. useful.
 

##different plots 
  par(mfrow = c(1,1))
  plot(log(fitted(glm1)), log((ships2$incidents-fitted(glm1))^2),
     xlab = expression(hat(lambda)), ylab = expression((y-hat(lambda))^2))
abline(0, 1)
      # What do you think this plot shows?
dp <- sum(residuals(glm1, type = "pearson")^2)/glm1$df.res
summary(glm1, dispersion = dp)

summary(fitted(glm1))
##OR write it as
glm2 <- update(glm1, family = quasipoisson)
summary(glm2)
anova(glm2, test="F")

par(mfrow = c(2,2))
plot(glm2)

plot(xtabs(abs(residuals(glm2)) ~ type + year))
plot(xtabs(incidents/service ~ type + year))
par(mfrow = c(1,1))
plot(glm2$fitted, glm2$y)

glm3 <- update(glm2, . ~ . - period, family = quasipoisson)
X11()
par(mfrow = c(2,2))
plot(glm3)

plot(residuals(glm3) ~ predict(glm3, type = "response"),
     xlab = expression(hat(mu)), ylab = "Deviance residuals")

plot(residuals(glm3) ~ predict(glm3, type = "link"),
     xlab = expression(hat(eta)), ylab = "Deviance residuals")

plot(residuals(glm3, "response") ~ predict(glm3, type = "link"),
     xlab = expression(hat(eta)), ylab = "Response residuals")

####From Julia's talk

# Create data frame containing the response variables deaths, 
#two explanatory factors nurse and  morning, and the variable shifts, 
#the number of shifts for that row of the table, used on a log-scale as
# an offset since we are modelling rates of deaths per unit time.

shifts<-c(8,7,2,28)
nurse<-as.factor(c('yes','yes','no','no'))
morning<-as.factor(c('yes','no','yes','no'))
deaths<-c(7,3,2,4)
data<-data.frame(shifts,morning,nurse,deaths)
print(data)
# Fit Poisson log-linear models for rates of death, both with just nurse
#included as an explanatory variable, and with morning also included. 
#Print analysis of deviance table and fitted values in each case.

fitN<-glm(deaths~nurse+offset(log(shifts)),
          family=poisson(),data)
summary(fitN)
print(anova(fitN,test='Chisq'))

##Is the nurse effect significant?


# Analysis of deviance table where only nurse is fitted. 
#Note that the p-value for the nurse effect is
# 0.01728, ie 1.7%, so apparently statistically significant.
print(fitted(fitN))
print(deaths)

##are the  fitted close to the observed n. deaths?


##Now let's include morning effect.

fitMN<-glm(deaths~morning+nurse+offset(log(shifts)),
           family=poisson(),data)
print(anova(fitMN,test='Chisq')) ##Comment
summary(fitMN)
# Analysis of deviance table where morning and nurse are both fitted. 
#Note that the p-value for the ###nurse effect ### is now 0.37849, i.e. 37.8%, 
#so it is not statistically significant.

print(fitted(fitMN))
print(deaths)
##comments

###Now you run the  Biased investigation and comment
# This proceeds in exactly the same way, but using the biased data
deaths<-c(8,4,1,3)
#as the input.
