# Example 4.1-4.5: Delivery time data (Model checking)
#
# This example illustrates how to compute and plot various types of residuals
#

### [1] Reading data into R and checking data
#
#   see the previous R script for file handling information.

rm(y,x1,x2)

data2=read.table(file="data-ex-3-1 (Delivery Time).prn", header=TRUE)  
attach(data2)  # data2 is a table in R. This allows variables in the table to be used individually.
print(data2)   # see if there data points are correct

library(scatterplot3d)
library(robustbase)

### [2] Plotting/visualizing data
#
#
par(mfrow=c(2,2))  # create a 2x2 frame to hold 4 plots
plot(x1,y)         # view linear relationship between x1 and y
plot(x2,y)         # view linear relationship between x2 and y
scatterplot3d(x1,x2,y) # view linear relationship between y and x1, x2:
                       # the points should be roughly on a plane.
plot(x1,x2)        # make sure x1 and x2 are not linearly related


### [3] Fitting a multiple linear regression model and compute residuals

fit1=lm(y~x1+x2) # fit the full model with both predictors

#[3a] compute/retrive the raw (oridinary) residuals e1

e1=fit1$residuals

#[3b] compute the standardized residuals d1

MSE=sum(e1^2)/(length(y)-3) # or MSE=(summary.lm(fit1)$sigma)^2 
d1=e1/sqrt(MSE)

#[3c] compute the studentized residuals r1

X=matrix(1,25,3)
X[,2]=x1
X[,3]=x2

XX=t(X)%*%X
invXX=solve(XX)
H=X%*%invXX%*%t(X)  # the H matrix
diagH=diag(H)       # diagonal elements of H

r1=e1/sqrt(MSE*(1-diagH)) 

#[3d] compute the PRESS residuals PRESSe

PRESSe=e1/(1-diagH)

#[3e] compute robust regression residuals r2

library(robustbase)    #load package "robustbase"
robfit=lmrob(y~x1+x2)  #robust regression

r2=robfit$residuals
s=robfit$scale
r2=r2/s

#display all data points, residuals and diagH

Resid=cbind(y, x1, x2, diagH, e1, d1, r1, PRESSe, r2)
Resid=round(Resid, digits=3)  #keeps only 3 digits after the decimal point
                              #for easy display
Resid

### [4] Plot the residuals ###########################################

#[4a] Four plots for residuals e1

par(mfrow=c(2,2))
qqnorm(e1)   #normal probability plot
qqline(e1)
shapiro.test(e1)  #normality test (optional)

fitted=y-e1
plot(fitted,e1,xlab="fitted values", ylab="residuals",ylim=c(-10,10))
abline(h=0)
title("residual vs fitted")
abline(h=3*sqrt(MSE), col="blue")
abline(h=-3*sqrt(MSE), col="blue")

plot(x1,e1,ylim=c(-10,10))
abline(h=0)
title("residual vs regressor x1")
abline(h=3*sqrt(MSE), col="blue")
abline(h=-3*sqrt(MSE), col="blue")

plot(x2,e1,ylim=c(-10,10))
abline(h=0)
title("residual vs regressor x2")
abline(h=3*sqrt(MSE), col="blue")
abline(h=-3*sqrt(MSE), col="blue")


#[4b] The same four plots for robust residual r2. These can also be done
#     to other residuals. Try it yourself.

par(mfrow=c(2,2))
qqnorm(r2)   #normal probability plot
qqline(r2)
shapiro.test(r2)  #normality test (optional)

fitted1=y-r2
plot(fitted1,r2,xlab="robust fitted values", ylab="residuals",ylim=c(-10,10))
abline(h=0)
title("residual vs fitted")
abline(h=3*s, col="blue")
abline(h=-3*s, col="blue")

plot(x1,r2,ylim=c(-10,10))
abline(h=0)
title("residual vs regressor x1")
abline(h=3*s, col="blue")
abline(h=-3*s, col="blue")

plot(x2,r2,ylim=c(-10,10))
abline(h=0)
title("residual vs regressor x2")
abline(h=3*s, col="blue")
abline(h=-3*s, col="blue")

### 5. Regression with cleaned up data (remove outlier and refit model)

#delete observation 9 (the outlier) from the data set
Xdata=cbind(x1,x2)
Xdata1=Xdata[-9,]
y1=y[-9]

fit2=lm(y1~Xdata1)  # LSE without the outlier
robfit2=lmrob(y1~Xdata1) # robust regression without the outlier

#compare regression results with and without outliers

summary(fit1)$coef    #observe the substantial difference between the two LSE
summary(fit2)$coef    #results without the outlier.
summary(robfit)$coef  #robust regression automatically handles the outlier; so
summary(robfit2)$coef #not much difference between the two robust fits.








