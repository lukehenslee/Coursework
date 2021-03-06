###################################################

# FISH 604: Modern Appied Statistics for Fisheries
# Lab 2: Bootstrap variance estimation
#        Distributions
# Franz Mueter

# Last modified: August 31, 2021

###################################################

if(!("chron" %in% installed.packages())) install.packages("chron")
if(!("fields" %in% installed.packages())) install.packages("fields")
if(!("fitdistrplus" %in% installed.packages())) install.packages("fitdistrplus")
if(!("QRM" %in% installed.packages())) install.packages("QRM")

# Please work through this lab in small groups or on your own if you don't 
# have a partner/group to work with

# Embed your tested and cleaned-up code in the script file but clearly delineate it 
# from existing code, for example by adding horizontal delineators before and after
# your code and your responses. This is so you can easily find it later before 
# submitting:
# ----------------------------------------------
   # (your code & answers)
# ----------------------------------------------

# Submit one set of results by the end of lab per group (or individual, if working on your own).
# Submit ONLY your solutions in text format, rather than the full r script. Clearly number the 
# exercises, for example:

# Exercise 1:
    # your code & any written answers to questions

# Exercise 2:
    # your code & any written answers to questions


#######################################################################################################
# 1. Use a bootstrap to estimate the distribution of extreme wind events!

# The data we will be using are daily wind speed in the East-West direction (u winds)
# and in the N-S direction (v winds) at a location on the middle shelf of the eastern
# Bering Sea (centered at 57 N 165 W). 

# The daily data are contained in the comma-delineated file 'M2winds.csv' 
# (take a look at the file using a spreadsheet)
# Columns are 'Date', u.speed', and 'v.speed' (Winds speeds in m/s)

# Import the data: 
# Make sure you copy the data set from Canvas to your working directory first
# If you don't know what your current working directory is, use:
getwd()
M2 <- read.csv("M2winds.csv", as.is=T)  # 'as.is' suppresses importing of Date column as a factor 
# Or, if you don't know where the file is, use interactive browser:
# M2 <- read.csv(file.choose(), as.is=T)
head(M2)  # Examine first few lines of data

# The Date column is recognized as a string of dates and you can extract days, years, months, etc
# You may have to first run: install.packages("chron") - Make sure you're online!
library(chron)   # loads the chron package
years(M2$Date)   # FACTOR variable of years
months(M2$Date)
days(M2$Date)

# Compute Julian day (days from beginning of year) and save as an additional column in M2 (for plotting):
M2$Julian <- julian(months(M2$Date), days(M2$Date), 1900, origin=c(12,31,1899)) 

# Check result:
head(M2, 20)

# Exercise 1:
# Now compute the absolute wind speed using Pythagoras' theorem and add it as another 
# column ('speed') to the existing data frame! 

# ------ Solution----------------------------------------------------------------
# M2$speed     # Insert formula to compute wind speed from u and v winds!
M2$speed <- sqrt(M2$v.speed^2 + M2$u.speed^2) 
head(M2)
# -------------------------------------------------------------------------------

# Cross-tabulate the data by year and Julian day and save as a new matrix:
# Note that there is only one value per day, hence the function 'mean'
# simply returns that value.
X <- tapply(M2$speed, list(years(M2$Date), M2$Julian), mean)

# Learn about the tapply function (?tapply) and examine the resulting object: 
X       # The whole thing (rather large!)
View(X) # To look at data in matrix form: years in rows  

dim(X)  # Number of rows and columns
X[,1]   # First column: 
X[1,]   # First row

# Display the data using color palette 
# (this is for illustration only, don't worry about figuring out the code - 
#  I simply provided it for reference)
library(fields)   

par(mar=c(3,4,4,6))
image(1:365, 1948:2009, t(X), col=tim.colors(64), xlab="", ylab = "", main = "Daily wind speeds (m/s), 1949-2009")
# Add a legend:
image.plot(1:365, 1948:2009, t(X), xlab="", ylab = "", yaxt="n", legend.only=T, col=tim.colors(64))

# All of the above can be done more efficiently and (to some) more
#  intuitively using the tidyverse, which has it's own read_csv 
#  file (package 'readr') that works slightly differently:
library(tidyverse)
M2.tbl <- read_csv("M2winds.csv")
M2.tbl <- M2.tbl %>% transmute(Date, year = years(Date), julian = julian(months(Date), days(Date), 1900, origin = c(12, 31, 1899)),
                               speed = sqrt(u.speed^2 + v.speed^2)) 
ggplot(M2.tbl, aes(julian, year, fill = speed)) + geom_raster() +
  xlab("Day of Year") + ylab("Year") + 
  scale_fill_gradientn(colors=tim.colors(100))

# Note that the data frame displays differently and can behave 
# differently in computations:
M2.tbl  # You can use either M2 or M2.tbl for the exercises below

# Exercise 2: (No need to show any results)
# Interpret and discuss the output with your group!

# Examine the distribution of daily wind speeds across years for each month:
library(lattice)
histogram( ~ speed | months(Date), data = M2)
# Again, interpret and discuss output with your group!

# Alternatively, using ggplot:
library(ggplot2)
ggplot(M2, aes(speed)) + 
  geom_histogram(binwidth=2) +
  facet_wrap(~months(Date))

# Exercise 3:
# Remember that we were going to examine extreme wind events! To do so, we first extract 
# the maximum daily wind speed for each year:
W <- apply(X,1,max)  # This applies the function 'max' to each row (=year) of the data and 
# returns the maximum observed wind speed for each year
sum(is.na(W))        # Check for missing values
W                    # Note missing value in 2009 (not all months had values)
W <- W[!is.na(W)]         # Omit missing value for 2009 (incomplete data)

# --------------------------- an optional alternative
# The 'tidyverse' approach (note that we don't need the 'X' matrix
# at all using the tidyverse syntax):
W <- M2.tbl %>% group_by(year) %>% summarize(Max = max(speed))
# This results in a data frame with two columns, but we 
# can extract ('pull out') the 'Max' column for inspection and
# for computations:
W %>% pull(Max)
# Note also that this includes the 2009 maximum:
tail(W)
# because the "long-format" data set (M2.tbl) does not have 
# any missing values in 2009 over which the maximum is computed

# Since the 2009 value may be biased, we'll remove it and save
# W as a simple vector:
W <- W %>% 
  dplyr::filter(year != 2009) %>% 
  pull(Max) 
#---------------------------

# (a) Plot a histogram or empirical density estimate of maximum annual wind speeds
# What does the distribution of extreme values look like? (symmetrical, skewed?)
# (Aside: The distribution of extreme events is often approximated by a "Gumbel" 
# distribution. We will get back to this at the end of the lab)

# ------ Solution-----------------------------------------------------
# use 'hist' or 'histogram' or 'geom_histogram' with ggplot
hist(W)

  # The histogram of extreme weather events appears to be approximately normally
  # distributed with a long right tail. The mean appears to be around 18 m/2 winds

skewness(W)
agostino.test(W)

  # According to our skewness test the data has a positive skew of about 1
# --------------------------------------------------------------------


# (b) Compute the 90th percentile of the distribution using the 'quantile' function 

# ------ Solution-----------------------------------------------------
?quantile

cat("The 90th percent quantile of the distribution is", quantile(W, 0.9))
# --------------------------------------------------------------------

# The 90th percentile may be taken as the magnitude of a once in a decade wind event 
# (1 out of 10 years have a wind event as large or larger)


# c) Using a bootstrap approach, we will estimate and visualize the distribution of a 
# once-in-a-decade wind event and calculate the mean and variance of its bootstrap 
# distribution.

# Adapt the code for bootstrapping the median (see 'Quick intro to bootstrap') to 
# bootstrapping the 90th percentile. You can simply replace the call to 'median' 
# with an appropriate call to 'quantile'to compute the 90th percentile.

# -----Solution-----------------------------------------------------------------
quant<- vector() # Create empty vector

for(i in 1:999) {
  boot <- sample(W, replace = T)
  quant[i] <- quantile(boot, 0.9) # Find 90th percentile of bootstrap sample and
    # add to vector 'quant'
}

mean(quant) # Mean value of 90th percentile of W
var(quant)
sd(quant)

  # Adapted function from 'Quick intro to bootstrap'
boot.med <- function(x, B)
  {
  # non-parametric bootstrap of the median
  res <- rep(NA, B)
  for(i in 1:B) res[i] <- median(sample(x, replace = T))
  list(boot.90th <- quantile(res, 0.9))
  }

boot.med(W, 1000)

# ------------------------------------------------------------------------------

#' Make sure to understand the bootstrap code! The mean of the 
#' bootstrapped quantiles (call it 'theta') can be taken as an estimate of the
#' average magnitude of a 'once-in-a-decade' wind event and the standard 
#' deviation is an estimate of the associated uncertainty. Compute two 
#' possible confidence intervals for our 'once-in-a-decade' winds: 
#'  1. One possibility is to assume that the estimates are approximately 
#'     normally distributed, hence we can estimate a 95% confidence interval 
#'     as: theta  +/- 1.96 *sd(theta)  (Why?)
#'  2. A second possibility is to use the central 95% of the bootstrap 
#'     distribution (rather than a normal distribution) as a confidence 
#'     interval. This requires computing the 2.5th and 97.5th percentiles 
#'     of the bootstrapped values in 'out', which you can compute using 
#'     the 'quantile' function.
#' How do the confidence intervals compare? Which one is more sensible?

# -----Solution-----------------------------------------------------------------

theta <- mean(quant)

# 1. Assume estimates are approximately normally distributed. 
  # The Central Limit Theorem states that the average of a large number of 
  # independent variables approximates a normal distribution. Therefore a 
  # bootstrap estimation can be assumed to be approximately normally distributed

cat("A 95% confidence interval for theta, assuming normal distribution, is",
    theta + c(-1.96, 1.96 * sd(quant)))

# 2. Use central 95% of bootstrap distribution.
cat("A 95% confidence interval for theta, using bootstrap estimates, is", 
    quantile(quant, c(0.025, 0.975)))

hist(quant)

  # When we visualize a histogram for the output of the bootstrap sample, the 
  # distribution doesn't appear to be normally distributed. 

skewness(quant)

  # The skewness test indicates a long right tail, which we also see in the 
  # histogram. It may be more reasonable to use the central 95% percent of the
  # bootstrap distribution, which has a higher confidence interval for once-in-
  # a-decade winds.

min(quant)

  # By the way, the lower bound of the 95% CI using the assumption of normality
  # is an entire m/s lower than the lowest observed bootstrap estimate. 
# ------------------------------------------------------------------------------


#######################################################################################################

# Distributions

# We covered a number of discrete distributions in class. Each of these distributions are 
# implemented in R and you can easily compute probabilities, quantiles corresponding to 
# certain probabilities, and generate random numbers from these distributions using the
# following built-in functions.

# Most distributions have four functions associated with them
# that serve the following basic purposes:

# The functions start with 'r', 'd', 'p', and 'q', followed by the (abbreviated) 
# name of the distribution, for example for the normal distribution:
dnorm()  # Compute density (pdf) of normal distribution
pnorm()  # Compute cumulative pdf of   "         "
qnorm()  # Compute quantiles of normal distribution 
rnorm()  # Generate random numbers from normal distribution

# Functions for the discrete distributions are:
# dbinom, pbinom, qbinom, rbinom                        (Bernoulli &) Binomial distribution
# dmultinom, rmultinom (no pmultinom and qmultinom)     Multinomial
# dpois, ... (etc.)                                     Poisson
# dnbinom                                               Negative binomial

# We explored the Bernoulli & binomial distributions, and will encounter the Poisson
# distribution again later!


#################################################################################### 
###  Multinomial distribution example

# Example: Long-term frequencies suggest that of the sockeye salmon returning to 
# Bristol Bay, 22% are 3 years old, 63% are 4 years old, and 15% are 5 years old 
p <- c(0.22, 0.63, 0.15)

# Say we catch 3 fish. What is the probability of catching exactly one fish of 
# each age? We can use the formula that I showed in class or use:
dmultinom(c(1,1,1), size=3, prob=p)

#### Exercise 4:
# What is the probability of a sample of 8 fish consisting of one (1) age-3, 
# four (4) age-4, and three (3) age-5 fish given the above probabilities (p)?
# Do the computation both "by hand" (i.e. based on the definition of the multinomial 
# probability distribution given in class (use "factorial")), 
# as well as using the 'dmultinom()' function

# ------ Solution-----------------------------------------------------

# 'By hand"
n <- 8
n3 <- 1
n4 <- 4
n5 <- 3

(prX <- (factorial(n)/prod(factorial(c(n3, n4, n5)))) * (p[1]^n3 * p[2]^n4 * p[3]^n5))

# Using 'dmulinom()' 
dmultinom(c(n3, n4, n5), size = n, prob = p)

# Both methods agree
# --------------------------------------------------------------------

# Because there are lots of different possible combinations of fish at each age if
# we have more than 3 fish, it is generally not simple (and not particularly useful) 
# to compute all of the corresponding probabilities. However, for n=3 fish in r=3
# age classes as in the above example, here is how you could do it if you had to 
# (generalizes to more than 3, but for simplicity it is illustrated here for 
#  this simple case):
# (FYI only, no need to work through this in class - examine output at each step)
(X <- t(as.matrix(expand.grid(0:3, 0:3)))) 
(X <- X[, colSums(X) <= 3])
(X <- rbind(X, 3:3 - colSums(X)))
dimnames(X) <- list(paste("age",1:3,sep="-"), NULL)
X
# We can compute the probability of getting each combination of 3 fish
# (given p) by applying 'dmultinom()' to each column of X:
round(apply(X, 2, function(x) dmultinom(x, prob = p)), 3)
# These probabilities should sum to 1 since there are only these 10 
# possible combinations

#### Exercise 5:
# Drawing random numbers from a multinomial distribution:
# Given the above probabilities (p), use the rmultinom() function to draw 
# 100 random multinomial age distributions that you might expect in a 
# sample of 50 salmon. Save the result!

# Use the results to compute the average number of fish by age class across 
# all 100 samples (you can use the apply function - type ?apply to find out more!)
# as well as the standard deviation of numbers at age

# ------ Solution-----------------------------------------------------
mult.salmon <- rmultinom(100, 50, p)

?apply

apply(mult.salmon, 1, mean)
apply(mult.salmon, 1, sd)

# --------------------------------------------------------------------


#################################################################################
### More on the Gumbel distribution and defining distributions:

# As noted above, the distribution of extreme values is often approximated by a
# 'Gumbel' distribution. We can check if the Gumbel distribution provides a 
# reasonable approximation to the observed wind speed maxima.

# There is no built-in 'Gumbel' distribution in the R base packages!
# However, we can easily define the required functions if we know
# the formulas for the corresponding pdf, cdf and quantile function
# (which are easily found online or in stats books):

dgumbel <- function(x,mu,s){ # PDF
  exp((mu - x)/s - exp((mu - x)/s))/s
}

pgumbel <- function(q,mu,s){ # CDF
  exp(-exp(-((q - mu)/s)))
}

qgumbel <- function(p, mu, s){ # quantile function
  mu-s*log(-log(p))
}

# Recall our maximum wind speeds:
W
# Using the above functions, we can estimate the parameters of the distribution
# that best fit our wind speed data using a function in the 'fitdistrplus' 
# package called 'fitdist'. We use maximum likelihood estimation (mle, there
# are several other options) and need to provide reasonable starting values:
library(fitdistrplus)
gumbel.fit <- fitdist(W, "gumbel", start=list(mu=mean(W), s=sd(W)), method="mle")
summary(gumbel.fit)

# Let's check the approximation visually:
hist(W, freq=F)
x <- seq(16,24, by=0.2)  
lines(x, dgumbel(x, 18.33, 1.046), col=2, lwd=2)  

# Or we can use the provided function for assessing goodness-of-fit:
plot(gumbel.fit, lwd=2, col="steelblue")

# The Gumbel distribution appears to approximate the distribution of maximum
# wind speeds quite well

# For comparison, let's fit a log-normal distribution, which is also right-skewed:
lognormal.fit <- fitdist(W, "lnorm", start=list(meanlog=mean(W), sdlog=sd(W)), method="mle")
summary(lognormal.fit)
plot(lognormal.fit)
# Because both models are fit using maximum likelihood, we can use the Akaike Information 
# Criterion (AIC, more on that later in class), to compare the two fits. 
# Note that the Gumbel distribution fits slightly better than the 
# log-normal (lower AIC by about 2.3)


# A little searching online reveals that the Gumbel distribution has been 
# implemented in the QRM package (among others): 
library(QRM)
hist(W, freq=F)
lines(x, dGumbel(x, 18.33, 1.046), col=2, lwd=2)  
# where 'dGumbel()' computes the density of the Gumbel distribution
# (spelled with upper-case 'G') - the result is identical to 
# what we get using our own function 'dgumbel()'!

