#########################################################################
# An idealized example of using a variance function to estimate the
# relationship between variance and predicted means (fitted values):

# Franz Mueter
# Last modified: August 23, 2021 

#########################################################################

### Simulate data whose variance increases with the fitted values
# Assume some linear relationship between x (ranging from 1-100) and y:
x <- 1:100
y <- 3 + 0.6*x
plot(x,y)
# Add residuals whose variance increases linearly with y
# (hence the standard deviation increases linearly with
#  the square root of y):
set.seed(123)
r <- rnorm(100, mean=0, sd=2*sqrt(y))
y <- y + r
plot(x,y) # Simulated data with increasing variance
# Because the variance increases linearly with the predicted 
# values, the ideal weights in a weighted regression would 
# be inverse proportional to 1/y 

# Of course with "real" data we don't typically know the relationship
# between the variance and the mean, hence we may have to estimate it

# We can do so "externally" (separate from the model) by examining 
# the mean-vairance relationship as shown in class, or we can fit
# a linear regression while simultaneously estimating 
# the underlying variance structure:

# Using the 'nlme' package (and other packages) We can specify a 
# variance structure that models the variance as increasing with 
# an (unknown) power of the fitted values. We do so using the 
# 'varPower' variance structure. Using this form for
# the variance function (proportional to some power of the 
# fitted values) is in the default for gls:
library(nlme)
(fit <- gls(y~x, weights=varPower(form= ~fitted(.))))
# The variance structure is specified via the 'weights'
# argument because the variance function (such as 'varPower' 
# or other available variance structures) sets up a diagonal 
# matrix of weights appropriate to the assumed structure (such 
# as the matrices I showed in class). 'varPower' uses a single 
# parameter t to determine the weights for each observation
# as shown below. The algorithm for a weighted regression is 
# iterative and in this case the fitted values are updated 
# at each iteration, thus the weights also change on each 
# iteration (until convergence)

# Weighted regression results using the estimated weights:
summary(fit)  
# The only 'new' component in the output compared to an OLS is 
# the estimated parameter for the power function, which is the 
# parameter t in the exponent of the following variance function,
# where v is a variance covariate, in this case the fitted values: 
#              var(v) = v^(2*t) 

# The estimated t is close to 0.5, thus the variance in this 
# case increases linearly with the fitted values, v 
# (i.e. it is proportional to v^1)

# The default diagnostic plot for a 'gls' object plots the standardized 
# (i.e. weighted) residuals against the fitted values, which confirms
# that the variance of the weighted residuals is approximately 
# constant across the range of fitted values:
plot(fit)  

# We can extract the inverse of the standard deviations that correspond
# to the estimated variance function using the 'varWeigths' function, which
# is used on the 'modelStruct' component of the output. This component
# specifies the variance structure and includes the estimate of the 
# weighting parameter:
fit$modelStruct                  # Model structure
varWeights(fit$modelStruct)      # Inverse of standard deviations

# The corresponding standard deviations are therefore simply the inverse 
# of the returned weights:
(SD <- 1/varWeights(fit$modelStruct))
SD^2
# We can confirm that the variance increases approximately linearly with 
# the fitted values by plotting the variance against the fitted values:
plot(fitted(fit), SD^2)
abline(0,1,col=2)
# This is not quite a straight line because the variance increases 
# as a power of the fitted values (2 * t = 2*0.4775), which is slightly
# smaller than 1:
lines(fitted(fit), fitted(fit)^(2*0.4775), col=3, lwd=2)
# It would be a perfectly straight line with a power parameter of 0.5

# A power parameter of t=1 corresponds to variances increasing with 
# the square of the fitted values v^(2*t) because of the parameterization
# used by 'varPower (see help file for 'varPower' or 'varClasses')

# For other ways to model variances (or standard deviations) as a 
# function of different covariates, see package 'lmvar':
# https://cran.r-project.org/web/packages/lmvar/vignettes/Intro.html


