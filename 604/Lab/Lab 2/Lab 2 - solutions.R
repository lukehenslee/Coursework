###################################################

# FISH 604: Modern Appied Statistics for Fisheries
# Lab 2: Bootstrap variance estimation
#        Distributions
# Franz Mueter

# Last modified: August 31, 2021

###################################################



#######################################################################################################
# 1. Use a bootstrap to estimate the distibution of extreme wind events!

# The data we will be using are daily wind speed in the East-West direction (u winds)
# and in the N-S direction (v winds) at a location on the middle shelf of the eastern
# Bering Sea (centered at 57 N 165 W). 

# The daily data are contained in the comma-delineated file 'M2winds.csv' 
# (take a look at the file using a spreadsheet)
# Columns are 'Date', u.speed', and 'v.speed' (Winds speeds in m/s)

# Import the data: 
# Make sure you copy the data set from Blackboard to your working directory first
# If you don't know what your current working directory is, use:
getwd()
M2 <- read.csv("M2winds.csv", as.is=T)  # 'as.is' suppresses importing of Date column as a factor 
# Or, you can open a browser to find the csv file:
M2 <- read.csv(file.choose(), as.is=T)
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
# Now compute the absolute wind speed using Pythagoras' theorem and add it as another column ('speed')
# to the exisiting data frame! 

# ------ Solution-----------------------------------------------------
  M2$speed <- sqrt(M2$v.speed^2 + M2$u.speed^2)
# --------------------------------------------------------------------


# Cross-tabulate the data by year and Julian day and save as a new matrix:
# Note that there is only one value per day, hence the function 'mean'
# simply returns that value.
X <- tapply(M2$speed, list(years(M2$Date), M2$Julian), mean)

# Learn about the tapply function (?tapply) and examine the resulting object: 
X       # The whole thing (rather large!)
View(X) # To look at data in matrix form: years in rows  

dim(X)  # Number of rows and columns
X[,1]   # First column: 
X[1,]   # first row

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
M2.tbl  # You can use either M2 or M2.tbl below


# Exercise 2: (No need to show any results)
# Interpret and discuss the output with your group!

# Examine the distribution of daily wind speeds across years for each month:
library(lattice)
histogram( ~ speed | months(Date), data = M2)
# Again, interpret and discuss output with your group!

# Alternatively, using ggplot:
ggplot(M2.tbl, aes(speed)) + 
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
W <- W %>% filter(year != 2009) %>% pull(Max) 
#---------------------------

# (a) Plot a histogram or empirical density estimate of maximum annual wind speeds
# What does the distribution of extreme values look like? (symmetrical, skewed?)
# (Aside: The distribution of extreme events is often approximated by a "Gumbel" 
# distribution. We will get back to this at the end of the lab)

# ------ Solution-----------------------------------------------------

  hist(W, freq=F)
  lines(density(W), col=4, lwd=2) # Kernel density estimate of pdf
  # Distribution is right-skewed

  # The tidyverse solution (replaces need for a call to 'apply' above):
  M2.tbl %>% group_by(year) %>% summarize(max.wind = max(speed)) %>% 
    ggplot(aes(max.wind)) + 
    geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) 
  # The function for 'binwidth' is based on the robust version of 
  # bin width suggested by Freedman and Diaconis (1981)
  
# --------------------------------------------------------------------


# (b) Compute the 90th percentile of the distribution using the 'quantile' function 

# ------ Solution-----------------------------------------------------
  quantile(W, 0.9) 
  
  # or:
  M2.tbl %>% group_by(year) %>% summarize(max.wind = max(speed)) %>% 
    pull(max.wind) %>% quantile(0.9)
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
  
  out <- numeric(999)
  for(i in 1:999) {
    W.boot <- sample(W, replace=T)
    out[i] <- quantile(W.boot, 0.9) 
  }
  hist(out)
  mean(out)  # Bootstrap estimate of 90% quantile
  sd(out)    # Bootstrap estimate of standard error of 90% quantile
  
# ------------------------------------------------------------------------------
  
#' c) Exercise: Work through and understand the bootstrap code! The mean of the 
#'    bootstrapped quantiles (call it 'theta') can be taken as an estimate of the
#'     average magnitude of a 'once-in-a-decade' wind event and the standard 
#'     deviation is an estimate of the associated uncertainty. Compute two 
#'     possible confidence intervals for our 'once-in-a-decade' winds: 
#'  1. One possibility is to assume that the estimates are approximately 
#'     normally distributed, hence we can estimate a 95% confidence interval 
#'     as: theta +/- 1.96 * sd(theta)  (Why?)
#'  2. A second possibility is to use the central 95% of the bootstrap 
#'     distribution (rather than a normal distribution) as a confidence 
#'     interval. This requires computing the 2.5th and 97.5th percentiles 
#'     of the bootstrapped values in 'out', which you can compute using 
#'     the 'quantile' function.
  
# -----Solution-----------------------------------------------------------------

  # Normal-based confidence interval:
  (ci1 <- mean(out) + c(-1.96, 1.96) * sd(out))
  abline(v=ci1, col=4, lwd=2)
  
  # Quantile-based confidence interval:
  (ci2 <- quantile(out, c(0.025, 0.975)))
  abline(v=ci2, col=2, lwd=2)
  
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

f <- factorial  # Create a copy of factorial with a short name for simplicity
k <- c(1,4,3)
f(sum(k)) / (f(k[1])*f(k[2])*f(k[3])) * p[1]^k[1] * p[2]^k[2] * p[3]^k[3]
# or, a much more compact version:
f(sum(k)) / prod(f(k)) * prod(p^k)

# compare to:
dmultinom(c(1,4,3), prob=p)

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
(X <- rbind(X, 3 - colSums(X)))
dimnames(X) <- list(paste("age",1:3,sep="-"), NULL)
X
# We can compute the probability of getting each combination of 3 fish
# (given p!) by applying 'dmultinom()' to each column of X:
round(apply(X, 2, function(x) dmultinom(x, prob = p)), 3)
# These probabilities should sum to 1 since there are only these 10 
# possible combinations

  
#### Exercise 5:
# Drawing random numbers from a multinomial distribution:
# Given the above probabilities (p), use the rmultinom() function to draw 
# 100 random multinomial age distributions that you might expect in a 
# sample of 50 salmon. Save the result!

# Use the results to compute the average number of fish by age class across 
# all 100 samples (use the apply function - type ?apply to find out more!)
# as well as the standard deviation of numbers at age
  
# ------ Solution-----------------------------------------------------
rand <- rmultinom(100, size=50, prob=p)
rand
# Average numbers by age:
apply(rand,1,mean)
apply(rand,1,mean) / 50   # average proportion by age
apply(rand, 1, sd)  # Standard deviations of average numbers in each age class
# Both the mean proportions and their standard deviations 
# should be fairly close ot the theoretical values:
#   var(n*p) = n * p * (1-p)   # variances
#    sd(n*p) = sqrt(n * p * (1-p))
sqrt(50*p*(1-p))

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
gofstat(gumbel.fit, discrete=FALSE) # goodness-of-fit statistics
plot(gumbel.fit)
  
# The Gumbel distribution appears to approximate the distribution of maximum
# wind speeds quite well

# For comparison, let's fit a log-normal distribution, which is also right-skewed:
lognormal.fit <- fitdist(W, "lnorm", start=list(meanlog=mean(W), sdlog=sd(W)), method="mle")
summary(lognormal.fit)
gofstat(lognormal.fit, discrete=FALSE) # goodness-of-fit statistics
plot(lognormal.fit)
# Because both models are fit using maximum likelihood, we can use the Akaike Information 
# Criterion (AIC, more on that later in class), to compare the two fits. 
# Note that the Gumbel distribution fits slightly better than the log-normal (lower AIC by
# about 2.3)
  

# A little searching online reveals that the Gumbel distribution has been 
# implemented in the QRM package (among others): 
library(QRM)
hist(W, freq=F)
lines(x, dGumbel(x, 18.33, 1.046), col=2, lwd=2)  
# where 'dGumbel()' computes the density of the Gumbel distribution
# (spelled with upper-case 'G') and the result is identical to 
# what we get using our own function 'dgumbel()'!

  
