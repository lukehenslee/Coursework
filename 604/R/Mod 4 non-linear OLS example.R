##################################################

# Module 4: Estimation

# Fitting a non-linear model via OLS regression

# Franz Mueter 
# Last modified: August 23, 2021

# See also sheet 'OLS' in the spreadsheet:
# 'OLS WLS GLS comparison.xlsx'

##################################################

# Set paramters for "bold" display:
par(lwd=3, pch=16, cex=1.5, cex.axis=1.8, cex.lab=1.8, mar=c(4.5,4.5,1,1))

# Ordinary least-squares example:

x <- c(26.5, 29.8, 36, 27.1, 42.9, 11.5, 44.8, 46.2, 31.3, 14.3,
     15.9, 14.8, 47.7, 36.5, 18.3, 14.4, 43.6, 39.7, 22.9, 24.5,
     48.7, 22.9, 6.1, 35.3, 19.4)

y <- c(3.44, 3.53, 3.52, 3.53, 3.62, 3.23, 3.71, 3.59, 3.58, 3.21,
     3.38, 3.14, 3.66, 3.62, 3.42, 3.25, 3.71, 3.63, 3.29, 3.51, 
     3.63, 3.4, 2.92, 3.56, 3.36)

plot(x, y)

# We know that (a) the relationship is of the form: y = a* x^b
# For simplicity, let's also pretend that we know that a = 2.5
# fit trial model with different values of b:
curve(2.5*x^0.2, add=T)  # b = 0.2
curve(2.5*x^0.15, add=T)  # b = 0.15
curve(2.5*x^0.1, col=4, add=T)  # b = 0.1   (Not bad!!)
curve(2.5*x^0.11, add=T, col=2)  # b = 0.11  
curve(2.5*x^0.09, add=T, col=2)  # b = 0.09  

# Surely, b must be somewhere between 0.09 and 0.11!!

# Plot residual sum of squares for different values of b, assuming a = 2.5:
# Choose values of b for which to compute RSS for doing a
# one-dimensional "grid search":
b <- seq(0.09, 0.11, by = 0.0001)
b

# Residual sum of squares for b = 0.1:
y - 2.5 * x^0.1    # Residuals (observed y - predicted values)
(y - 2.5 * x^0.1)^2   # Squared residuals
sum((y - 2.5 * x^0.1)^2)  # Sum of squared residuals (RSS), to be minimized

# Our goal is to find the value for 'b' that leads to the smallest possible RSS

# Compute RSS for each trial value of b:
RSS <- rep(NA, length(b))   # Set up empty vector for results
RSS
for(i in 1:length(b)) {
  RSS[i] <- sum((y - 2.5 * x^b[i])^2)
}
cbind(b,RSS)    # RSS values for each b

# Plot RSS for each trial value of b:
plot(b, RSS, type="l")
# Find value that minimizes RSS:
b.hat <- b[which.min(RSS)]
abline(v=b.hat, col=2)

# The RSS is equivalent to the negative log-likelihood if we assume
# a normal distribution for the residuals
# This plot is also called the "likelihood profile" of b 
# (given 'a' and the data x,y)


# Plot fitted model:
plot(x,y)
curve(2.5*x^b.hat, add=T, col=4)  # Least-squares fit

# Diagnostic plot: Residuals against fitted values
plot(2.5*x^b.hat, y - 2.5*x^b.hat, xlab="fitted values", ylab="Residuals")
abline(h=0, lty=2)  # No evidence of heteroscedasticity or other residual patterns

# Note that we just fit a non-linear model via OLS regression!

# The uncertainty in the parameters is related to how strongly curved the
# likelihood profile is (i.e. the second derivative of RSS with respect to b!)

# For minimizing a function over a single parameter we can also use
# a gradient search method such as those implemented in 'optimize':
f <- function(b, X, Y) sum((Y - 2.5 * X^b)^2)    # Define function to minimize (RSS)
optimize(f, interval=c(0.09, 0.11), X=x, Y=y)    # Optimize function in the interval between 0.09 and 0.11
# 'minimum' is the estimated value for b and 'objective' is the corresponding minimum RSS

# To minimize function over SEVERAL parameters, use 'optim':
# find best values for both a and b in relationship: y = a*x^b
# (i.e., minimize over both a and b) 
# In this formulation, I use a vector 'b' for the parameters, where 
# b[1] corresponds to intercept a and b[2] corresponds to slope b !
f <- function(b, X, Y) sum((Y - b[1] * X^b[2])^2)
b.hat <- optim(c(2.5, 0.1), f, X=x, Y=y)
b.hat

plot(x,y)
curve(b.hat$par[1]*x^b.hat$par[2], add=T, col=4)  # Least-squares fit

# For non-linear model fitting we can also use the 'nls' function, 
# which has a different syntax:
fit.nls <- nls(y ~ a * x^b, start = list(a = 2.5, b = 0.1))
# where the first argument is a formula with the response on the
# left-hand side (LHS) and the predicted value on the RHS. 
# 'nls' minimizes the squared differences between the LHS and RHS  

# Summary of results from nls fitting:
summary(fit.nls)
(cf <- coef(fit.nls))   # Extract and save coefficients
curve(cf[1]*x^cf[2], col=2, add=T)

# Parameter estimates are almost identical to 'optim'
cf    # 'nls' estimates
b.hat$par   # 'optim' estimates


# We will follow up on this examples of a weighted least-squares 
# regression and generalized least-squares regression in the Lab


