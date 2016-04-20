### FITTING A LINEAR MODEL BY FORCING THE INTERCEPT TO BE CERO ###

https://stat.ethz.ch/pipermail/r-help/2007-August/139514.html

# Assuming you want a model with an intercept of zero, I think we need to
# ask you why you want an intercept of zero. When a "normal" regression
# indicates a non-zero intercet, forcing the regression line to have a
# zero intercept changes the meaning of the regression coefficients. If
# for some reason you want to have a zero intercept, but do not want to
# change the meaning of the regression coefficeints, i.e. you still what
# to minimize the sum of the square deviations from the BLUE (Best
# Leastsquares Unibiased Estimator) of the regression, you can center your
# dependent and indepdent variables re-run the regression. Centering means
# subtracting the mean of each variable from the variable before
# performing the regression. When you do this, the intercept term will be
# zero (or more likely a very, very, very small number that is not
# statisitclally different from zero - it will not be exactly zero due to
# limits on the precision of computer calculations) and the slope term
# will be the sam!
# e as that you obtained from the "normal" BLUE regression. What you are
# actually doing is transforming your data so it is centered around x=0,
# y=0, i.e. the mean of the x and y terms will be zero. I am not sure this
# is what you want to do, but I am pasting below some R code that will
# allow you to see the affect fourcing the intercept to be zero has on the
# slope, and how centering the data yields a zero intercept without
# changing the slope.

oldpar<-par(ask=T)

# Set up x and y values. Note as defined the slope of the 
# regression  should be close to one (save for the "noise"
# added to the y values) and the intercept should be close to four.
x<-0:10
y<-x+4+rnorm(11,0,1)
plot(x,y, ylim=c(0,15))
title("Original data")

# Fit a "normal" regression line to the data and display 
# the regression line on the scatter plot
fitNormalReg<-lm(y~x)
abline(fitNormalReg)

# Fit a regression line in which the intercept has been 
# forced to be zero and display the line on the scattter 
# plot.
fitZeroInt<-lm(y~-1+x)
abline(fitZeroInt,lty=2)

# Compare fits. 
summary(fitNormalReg)
summary(fitZeroInt)
# There is a statistically significant difference 
# between the models the model with and intercetpt, 
# the "normal" regression is the better fit.
anova(fitNormalReg,fitZeroInt)

# Center y and x by subtracting their means.
yCentered<-y-mean(y)
xCentered<-x-mean(x)
# Regress the centered y values on the centered x values. This 
# will give us a model with an intercept that is very, very 
# small. It would be zero save for the precision limits 
# inherent in using a computer. Plot the line. Notice the 
# slope of the centered is the same as that obtained from 
# the normal regression. 
fitCentered<-lm(yCentered~xCentered)
abline(fitCentered,lty=10)

# Compare the three regressions. Note the slope from the 
# "normal" regression and centered regressions are the same.
# The intercept from the centered regression is very, very small 
# and would be zero save for the limits of computer mathematics.
summary(fitNormalReg)
summary(fitZeroInt)
summary(fitCentered)

# Plot the centered data and show that the line goes through zero.
plot(xCentered,yCentered)
abline(fitCentered)
title("Centered data")
oldpar<-par(ask=T)

