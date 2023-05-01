#####Hypothesis testing ###########
xbar = 2.1             # sample mean 
mu0 = 2                # hypothesized value 
sigma = 0.25           # population standard deviation 
n = 35                 # sample size 
a= sqrt(n)
z = (xbar-mu0) / (sigma/a ) 
z                      # test statistic 

#We then compute the critical value at 0.05 significance level.

alpha = 0.05 
z.alpha = qnorm(1-alpha) 
z.alpha # critical value 

#### 
pval = pnorm(z, lower.tail=FALSE) 
pval    # upper tail p−value 
########################one sample##
# create a vector with the data
daily.intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
# Test null hypothesis mu= 7727 kJ.
t.test(daily.intake,mu=7727)
################Wilcoxon########
wilcox.test(daily.intake, mu=7727)
Q= c(-0.57,-0.19,-0.05, 0.76, 1.30, 2.02, 2.17, 2.46, 2.68, 3.02)
wilcox.test(Q, mu=0)


#############Weed example##########
A<- c(166.7, 172.2, 165.0, 176.9)
B<- c(158.2, 176.4, 153.1, 106.0)
dat <- data.frame(yield = c(A,B), 
                  weed = rep(c("A","B"), each=4))
boxplot(yield ~ weed, data = dat)

wilcox.test(yield ~ weed, data = dat)
#####ANOVA##############
X1=rnorm(100, 10,2)
mean(X1)
sd(X1)
X2=rnorm(100, 20,2)
mean(X2)
sd(X2)
X3=rnorm(100, 30,2)
mean(X3)
sd(X3)
dat <- data.frame(ANOVA = c(X1,X2,X3), 
                  Pooled = rep(c("X1","X2","X3"), each=100))
boxplot(ANOVA ~ Pooled, data = dat)
lm2 <- lm(ANOVA~Pooled, data = dat)
anova(lm2)
##Non Parametric ANOVA #######
kruskal.test(ANOVA~Pooled, data = dat)
#########Two way ANOVA#####
set.seed(10)

#create data frame
data <- data.frame(gender = rep(c("Male", "Female"), each = 30),
                   exercise = rep(c("None", "Light", "Intense"), each = 10, times = 2),
                   weight_loss = c(runif(10, -3, 3), runif(10, 0, 5), runif(10, 5, 9),
                                   runif(10, -4, 2), runif(10, 0, 3), runif(10, 3, 8)))
head(data)
#load dplyr package
library(dplyr)

#set margins so that axis labels on boxplot don't get cut off
par(mar=c(8, 4.1, 4.1, 2.1))

#create boxplots
boxplot(weight_loss ~ gender:exercise,
        data = data,
        main = "Weight Loss Distribution by Group",
        xlab = "Group",
        ylab = "Weight Loss",
        col = "steelblue",
        border = "black", 
        las = 2 #make x-axis labels perpendicular
)
#The general syntax to fit a two-way ANOVA model in R is as follows:
#aov(response variable ~ predictor_variable1 * predictor_variable2, data = dataset)
#fit the two-way ANOVA model
model <- aov(weight_loss ~ gender*exercise, data = data)

#view the model output
summary(model)
#perform Tukey's Test for multiple comparisons
TukeyHSD(model, conf.level=.95) 
#############
#We can also visualize the 95% confidence intervals that result from the Tukey Test by using the plot() function in R:

#set axis margins so labels don't get cut off
par(mar=c(4.1, 13, 4.1, 2.1))

#create confidence interval for each comparison
plot(TukeyHSD(model, conf.level=.95), las = 2)
