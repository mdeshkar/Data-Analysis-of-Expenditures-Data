########################################################################
## Required packages and data for the Exploratory Data Analysis(EDA)
########################################################################

library(ISLR)
library(stargazer)
library(xtable)
library(leaps) # for regsubsets
library(car)   # for qqPlot
library(glmnet) # for lasso
library(gam)    # for generalized additive models
library(MASS)
library(vioplot)
library(car)
library(corrplot)

########################################################################
## Accessing the data and removing the records having missing values
########################################################################

rm(list=ls(all=TRUE)) # remove all previous objects from memory
ny<-read.table(choose.files(),header = T) # Accessing the data file form the system
sum(is.na(ny))# 2 of the records in the data set is having the missing value
ny2 <- na.omit(ny)# omit the missing values form the data set
attach(ny2)
options(warn=-1)# Get rid of warnings

########################################################################
## Review Data With Expense as a Response variable
########################################################################
names(ny2)
summary(ny2)
summary(expen)
stargazer(ny2[c("expen","wealth","pop","pint","dens","income","growr")], title="Summary statistics for the Newyork Muncipalities", label="descrips",summary.stat = c("mean","median","sd","min","max","p25","p75"),
          covariate.labels = c("Expenses","Wealth","Population","% Inter Governmental Funds","Density","Income","Growth Rate",flip=TRUE ))
## Stargazer is used to get the latex code that is used to obtain the table for the summary statistics
## in latex
head(ny2, n=5)
#1) most of the people are having expenditure below 320. 
#2) The Range is too wide as the data is too variable . 

########################################################################
##Graphics for the univariate explorations
########################################################################
boxplot(expen,xlab="Entity",ylab="Expenditure") # boxplot 
hist(expen,breaks=20,xlab="Expenditure",ylab = "Density",main = "")
vioplot(expen)  # combines box plot and kernel density estimate

# Code to get rid of that '1' on the x-axis and label the y-axis
plot(1, 1, xlim = c(0, 2), ylim = range(expen), type = 'n', xlab = '', ylab = '', xaxt = 'n')
vioplot(expen, at=1, add=T)  # combines box plot and kernel density estimate
axis(1, at = 1, labels = c('NY municipalities')) # set horizontal axis
axis(2, at = 1700, pos=-0.2, tck=0, labels="Expenditures")
qqnorm(expen) 
qqline(expen)

## it is evident form the plots that the expense variable is highly skewed towards the right
## In order to make the predictions plausiable for the regression analysis the response variable 
##should follow the  normality assumptions therefore we will try to use the transformations.

###Let's have a look at the log transformed expense variable
# histogram smoothing
lexpen = log(expen)
# histogram with density plot overlay
hist(lexpen, prob=T, breaks = 20, xlab="Log-Expenditures", ylab="Density", main="")
# density smooth
lines(density(lexpen), col="blue")
rug(lexpen) # data rug
# normal approximation
curve(dnorm(x, mean=mean(lexpen), sd=sd(lexpen)), add=TRUE, col="green")
## It seems that the log transformed works well and the bell cureve seems to be plausiable.
boxplot(lexpen,xlab="Entity",ylab="Log-Expenditure") # boxplot 
hist(lexpen,breaks=20,xlab="Log-Expenditure",ylab = "Density",main = "")
vioplot(lexpen)  # combines box plot and kernel density estimate

# Code to get rid of that '1' on the x-axis and label the y-axis
plot(1, 1, xlim = c(0, 2), ylim = range(lexpen), type = 'n', xlab = '', ylab = '', xaxt = 'n')
vioplot(lexpen, at=1, add=T)  # combines box plot and kernel density estimate
axis(1, at = 1, labels = c('NY municipalities')) # set horizontal axis
axis(2, at = 1700, pos=-0.2, tck=0, labels="Log-Expenditures")
qqnorm(lexpen) 
qqline(lexpen)

## From the qq-plot it seems that the extreme data points are moving away from the normal line 
##but all in alll it is not voilating our normality assumptions. Therefore we can argue for the 
## log-transformation

###########################################################
## Consider the transformations for the predictor variables
###########################################################
lpop<-log(pop)
ldens<-log(dens)
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
lpint<-log(pint)
pint2<-pint**2
pint3<-pint**3
lpop2<-lpop**2
lpop3<-lpop**3
ldens2<-ldens**2
ldens3<-ldens**3
i2 = income^2
i3 = income^3
#  Consider a piecewise transformation:
# Use different transformations for growth rates > 0 and for growth rates < 0.
# As long as the joint transformation is monotonic and keeps the zero at 0, we are fine.
# One approach is the transformation t(x), x>0; -t(-x), x<0. 
# Then can apply log-transformation!  
# But have to be careful with x=0 since ln(x)=ln(0) is undefined.  Easy fix: add 0.15.
# Create the piecewise function
lgrowr<-ifelse(growr>0, log(growr+0.15), -log(-growr+0.15))

lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))

# EDA for predictors, univariate analysis
# Let us consider log-transformations for all variables.
# We will the Use scatter plot smooths to study the relationship between log-expenditure and predictors
# a) wealth and log-wealth 
par(mfrow=c(1,2))
plot(wealth, lexpen, xlab="% Wealth", ylab="Log-Expenditure")
lines(smooth.spline(wealth,lexpen), col="blue")
abline(lm(lexpen~wealth), col="green")

plot(lwealth, lexpen, xlab="% Log-Wealth", ylab="Log-Expenditure")
lines(smooth.spline(lwealth,lexpen), col="blue")
abline(lm(lexpen~lwealth), col="green")

# b) population and log-population with a cut-point
#   here is code to do (b)
plot(pop, lexpen, xlab = "Population", ylab = "Log-Exense")  # notice need for a transformation on pop
lines(smooth.spline(pop,lexpen),col="blue")
abline(lm(lexpen~pop), col="green")

plot(lpop, lexpen, xlab = "Log-Population", ylab = "Log-Exense")
lines(smooth.spline(lpop,lexpen),col="blue")
abline(lm(lexpen~lpop), col="green")
lines(lowess(lpop,lexpen), col="blue")
lines(c(8.3,8.3),c(0,6), col="grey", lwd=3)


plot(dens, lexpen, xlab = "Density" , ylab = "Log-Expense")  # notice need for a transformation on pop
lines(smooth.spline(dens,lexpen),col="green")
abline(lm(lexpen~dens), col="blue")



plot(ldens, lexpen, xlab = "Log-Density" , ylab = "Log-Expense")
lines(smooth.spline(ldens,lexpen),col="green")
abline(lm(lexpen~ldens), col="blue")
lines(lowess(ldens,lexpen), col="blue")
lines(c(4.5,4.5),c(0,5.5), col="grey", lwd=3)



par(mfrow=c(1,2))
plot(growr,lexpen,xlab = "Log-Expen", ylab = "GrowthRate")
lines(smooth.spline(growr,lexpen),col="green")
abline(lm(lexpen~growr), col="blue")

plot(lexpen,lgrowr,xlab = "Log-Expen", ylab = "Log-GrowthRate")
lines(smooth.spline(lgrowr,lexpen),col="green")
abline(lm(lexpen~lgrowr), col="blue")

####################################################################
##Correlation plot to check the if there exist any Multicollinearity 
####################################################################
par(mfrow=c(1,1))
ny2vars = data.frame(expen, wealth, pop, pint, dens, income, growr)
cny2 = cor(ny2vars)
corrplot(cny2)



####################################################################
##Subsetting the data as per the increasing trend as our aim is
##to analyse the increasing trend 
####################################################################
set2<-ny2[which(lpop>8.3 & ldens>4.5),]
attach(set2)
set2<-set2[-211,]
set2<-set2[-226,]
set2 <- set2[-4,]
attach(set2)
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
lpint<-log(pint)
pint2<-pint**2
pint3<-pint**3
lpop2<-lpop**2
lpop3<-lpop**3
ldens2<-ldens**2
ldens3<-ldens**3
i2 = income^2
i3 = income^3
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))

plot(pop,lexpen,xlab = "Population",ylab="Log-Expenses",subset=set2)
lines(smooth.spline(pop,lexpen),col="green",subset=set2)
abline(lm(lexpen~pop),col="blue",subset=set2)

plot(lpop<-log(pop),lexpen, xlab = "Log-Population", ylab = "Log-Expenses",subset=set2)
lines(smooth.spline(lpop,lexpen),col="green",subset=set2)
abline(lm(lexpen~lpop),col="blue",subset=set2)

plot(dens,lexpen,xlab = "Density",ylab = "Log Expenses",subset=set2)
lines(smooth.spline(dens,lexpen),col="green",subset=set2)
abline(lm(lexpen~dens),col="blue",subset=set2)

plot(ldens<-log(dens),lexpen,xlab = "Log Density" , ylab = "Log Expenses",subset=set2)
lines(smooth.spline(ldens,lexpen),col = "green",subset=set2)
abline(lm(lexpen~ldens),col="blue",subset=set2)

plot(wealth,lexpen,xlab = "Wealth",ylab = "Log-Expenses",subset=set2)
lines(smooth.spline(wealth,lexpen),col="green",subset = set2)
abline(lm(lexpen~wealth),col="blue",subset=set2)

plot(lwealth <- log(wealth),lexpen,xlab="Log-Wealth",ylab = "Log-Expenses",subset=set2)
lines(smooth.spline(lwealth,lexpen),col="green",subset=set2)
abline(lm(lexpen~lwealth),col="blue",subset=set2)

plot(income,lexpen, xlab = "Income",ylab = "Log Expenses",subset=set2)
lines(smooth.spline(income,lexpen),col="green",subset=set2)
abline(lm(lexpen~income),col = "blue",subset=set2)

plot(lincome<-log(income),lexpen , xlab = "Log-Income",ylab="Log-Expenditure",subset=set2)
lines(lowess(lincome,lexpen),col = "green",subset=set2)
abline(lm(lexpen~lincome),col = "blue",subset=set2)

plot(pint , lexpen , xlab = "Inter Governmental Funds",ylab = "Log Expenses",subset=set2)
lines(smooth.spline(pint , lexpen) ,col ="green",subset=set2)
abline(lm(lexpen~pint),col="blue",subset=set2)

plot(lpint<-log(pint) , lexpen , xlab = "Log Inter Governmental Funds" 
     , ylab = "Log Expenses",subset=set2)
lines(smooth.spline(lpint,lexpen),col="green",subset=set2)
abline(lm(lexpen~lpint),col="blue",subset=set2)

plot(growr,lexpen,xlab = "Growth Rate",ylab = "Log-Expenditure",subset=set2)
lines(smooth.spline(growr,lexpen),col="green",subset=set2)
abline(lm(lexpen~growr),col="blue",subset=set2)

lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))

plot(lgrowr,lexpen,xlab = "Log-Growth Rate", ylab="Log-Expenditure",subset=set2)
lines(smooth.spline(lgrowr,lexpen),col="green",subset=set2)
abline(lm(lexpen~lgrowr),col="blue",subset=set2)


# For illustration purposes, let's create a wealth category variable.
wealthcat = 1*(wealth<40000)+2*(wealth>40000)*(wealth<80000)+3*(wealth>80000)*(wealth<100000)+4*(wealth>100000)*(wealth<200000)+5*(wealth>200000)
wealthcat=factor(wealthcat) # tell R this variable is categorical
# tabulate the categorical variable
table(wealthcat) 
# Parallel boxplots with box width a function of sample size
plot(wealthcat, expen, col="cyan", varwidth=T,
     ylab="Expenditures",xlab="Wealth Categories")


#############################################################
## Regression Model after subsetting the data
#############################################################
fit11<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
            ldens+ldens2+ldens3+income+i2+lgrowr,data = set2)
summary(fit11)
# remove income^2 (i2)
fit11b<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
             ldens+ldens2+ldens3+income+lgrowr,data = set2)
summary(fit11b)
# remove ldens
stepAIC(fit11, direction="both")  # get same final model as fit11b
###########################################################
## Model diagnostics: typically iterative process
###########################################################

# Residual plot
plot(predict(fit11b), rstudent(fit11b), ylab="Studentized Residuals", xlab="Predicted")
identify(predict(fit11b), rstudent(fit11b), labels=row.names(ny2)) # 'escape to finish'
predict(fit11b)[rstudent(fit11b)==min(rstudent(fit11b))]
#887 
#5.941401 
ny2[887,]
#obs st co expen wealth  pop pint dens income   id growr
#887 893 36 55    57 170195 5371 29.8 4883  34082 7340 -54.1

# Normality of Residuals
sresid <- studres(fit11b)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# Q-Q plot for studentized resid
qqPlot(fit11b, main="QQ Plot", ylab="Studentized Residuals")
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-p-1) as a guide; 
# Cook and Weisberg recommend 0.5 and 1 (R uses these guides in default diagnostic plots below)
cutoff <- 4/((nrow(ny2)-length(fit11b$coefficients)-2))
plot(fit11b, which=4, cook.levels=cutoff) # influence Plot
# Influence plot: studentized residuals vs. hat matrix diagonals (leverage) with bubbles a function of Cook's D
# Interactive, so can click to identify high leverage/influential/outlying points
influencePlot(fit11b, id.method="identify", 
              main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# VIF: don't want to put in polynomial terms since they are correlated!
fit=lm(lexpen~lwealth+lpint+ldens+income+lgrowr,data = set2)
vif(fit) # closer to 1 the better; 5-10 is moderate
#lwealth     lpop     pint    ldens   income   lgrowr 
#1.906337 6.413258 1.231470 7.854204 2.682954 1.022102 

# All encompansing R default regression model diagnostics
par(mfrow=c(2,2))
plot(fit11b)
## The regression Model seems to be resasonable according to the diagnostic plots.
fit <- lm(lexpen~lwealth + ldens + lpint + lgrowr, data = set2)# as population and density are correlated
# to each other it is better to use any one of it to avoid multi collinearity.

stargazer(fit, title="Regression Inference Of the Log-Expense Variable .", label="reginf", 
          align=TRUE,  ci=TRUE, ci.level=0.95, single.row=TRUE, omit.stat=c("LL", "ser", "f"),
          covariate.labels=c("Log Wealth", "Log Density", "Log Pint", "Log Growth Rate"
          ),
          dep.var.labels="Log Expenses", digits=2)
# this function is used to obtain the code for latex to get the regression table.



##################################################################
## Predictions for Warwick and Monroe using above regression model
##################################################################
sdfit=sd(fit11b$resid)
new05 = data.frame(lwealth=log(85000), lpop=log(20442), lpop2=log(20442)^2, lpop3=log(20442)^3, pint=24.7, 
                   pint2=24.7^2, pint3=24.7^3, ldens= log(214), ldens2=log(214)^2, ldens3=log(214)^3, income=19500, 
                   lgrowr=log(35+1))
new25 = data.frame(lwealth=log(89000), lpop=log(31033), lpop2=log(31033)^2, lpop3=log(31033)^3, pint=26.0, 
                   pint2=26.0^2, pint3=26.0^3, ldens = log(214), ldens2=log(214)^2, ldens3=log(325)^3, income=20000, 
                   lgrowr=log(40+1))
warick05=predict.lm(fit11b,new05); exp(warick05+sdfit^2/2)                 
exp(predict(fit11b, new05, interval="prediction")+sdfit^2/2)
warick25=predict.lm(fit11b,new25); exp(warick25+sdfit^2/2)  
exp(predict(fit11b, new25, interval="prediction")+sdfit^2/2)
#fit      lwr      upr
#1 266.9911 135.0772 527.7298
 warick25=predict.lm(fit11b,new25); exp(warick25+sdfit^2/2)  
#1 
#377.3172 
> exp(predict(fit11b, new25, interval="prediction")+sdfit^2/2)
#fit      lwr      upr
#1 377.3172 185.3709 768.0185

 ## From the given model above are the predictions for Warwick and Monroe 


