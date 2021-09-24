##########################################################
# FISH 604
# Homework 1 script file 

# Expected values and variances: Sockeye salmon fecundity

# Luke Henslee- 8/28/2021
##########################################################

# Load packages
library(tidyverse)

#### Problem 1:

### Import data:
f.dat <- read.csv("sockeye fecundity.csv", head=T)
str(f.dat)           # Examine data
table(f.dat$Lake)    # Number of observations by lake

### (a) Plot histograms and boxplots

  # Create dataframes for each lake
f.dat.1 <- dplyr::filter(f.dat, Lake == "Lake1")
f.dat.2 <- dplyr::filter(f.dat, Lake == "Lake2")

  # Lake 1
hist(f.dat.1$Fecundity) # High frequency fecundity of 3000-3500

  # Lake 2
hist(f.dat.2$Fecundity) # High frequency fecundity of 3500-4000

  # Boxplot
boxplot(Fecundity ~ Lake, f.dat) # Supports average fecundity higher in Lake 2

#--------------------------------------------------------------

# For further examining the data, it is useful to split the 
# fecundity vector into two components ('split' creates a list)
(f.dat2 <- split(f.dat$Fecundity, f.dat$Lake))


### (b) Compute mean fecundities (expected values) and their standard errors:

  # Lake 1
mean(f.dat2$Lake1) # Expected value of lake 1 fecundity
sd(f.dat2$Lake1) # Standard deviation of lake 1 fecundity
sd(f.dat2$Lake1)/sqrt(length(f.dat2$Lake1)) # Standard error of lake 1 fecundity
var(f.dat2$Lake1) # Variance of lake 1 fecundity 

  # Lake 2
mean(f.dat2$Lake2) # Expected value of lake 2 fecundity
sd(f.dat2$Lake2) # Standard deviation of lake 2 fecundity
sd(f.dat2$Lake2)/sqrt(length(f.dat2$Lake2)) # Standard error of lake 2 fecundity
var(f.dat2$Lake2) # Variance of lake 2 fecundity 
#--------------------------------------------------------------

### (c) Compare means 
# Check out help files for 't.test' and 'oneway.test'
# ?t.test
# ?oneway.test

#---- Enter your code below and briefly annotate as needed ----
t.test(f.dat.1$Fecundity, f.dat.2$Fecundity) # Reject hypothesis of no 
  # difference in mean fecundity

oneway.test(Fecundity ~ Lake, f.dat) # Both tests indicate a significant 
  # difference in mean fecundity between Lakes 1 and 2

  # Based on the results of these tests, fecundity estimates and 
  # standard errors should be reported separately by lake. There appears to be 
  # significant differences between populations and reporting a combined
  # fecundity would obscure true reproductive potential of both populations. 
#--------------------------------------------------------------

###############################################################
#### Problem 2:

### Input values needed for expected values and variance:

# Proportion of females by lake:
p1 <- 0.58 ## Enter appropriate values from table
p2 <- 0.52

# Expected numbers of total spawners:
E.N1 <- 124360 
E.N2 <- 73608

# Standard errors:
s.N1 <- 23245
s.N2 <- 10120

# Other quantities from 1b above
  # Expected number of eggs per female
E.f1 <- mean(f.dat2$Lake1)
E.f2 <- mean(f.dat2$Lake2)

  # Standard error
se.f1 <- sd(f.dat2$Lake1)/sqrt(length(f.dat2$Lake1))
se.f2 <- sd(f.dat2$Lake2)/sqrt(length(f.dat2$Lake2))

  # Standard deviation
s.f1 <- sd(f.dat2$Lake1)
s.f2 <- sd(f.dat2$Lake2)

### (a) Expected number of eggs (in millions):
# Note that pi is a constant, and Ni and fi are independent

#---- Enter your code below and briefly annotate as needed ----

  # Expected number of eggs is equal to the expected number of females (E.Ni *
  # pi) times the expected fecundity (E.fi) for each lake

E.egg1 <- p1 * E.N1 * E.f1 # Expected number of eggs for lake 1
E.egg2 <- p2 * E.N2 * E.f2 # Expected number of eggs for lake 2

E.egg <- E.egg1 + E.egg2 # Expected number of eggs for both lakes
E.egg/1e6 # In millions of eggs
#--------------------------------------------------------------


###  (b) Variance:

#---- Enter your code below and briefly annotate as needed ----

# The variance of the product of independent variables with a constant is given as:
# var(Ei) = var(pi * Ni * fi)
#         = pi^2 * (E(fi)^2 * var(Ni) + E(Ni)^2 * var(fi) + var(Ni) * var(fi))
#   E(Ni) = E.Ni; E(fi) = E.fi; var(Ni) = s.N1^2; and var(fi) = var.fi

var.egg1 <- p1^2 * (E.f1^2 * s.N1^2 + E.N1^2 * s.f1^2 + s.N1^2 * s.f1^2)
sd.egg1 <- sqrt(var.egg1)
sd.egg1/1e6 # In millions of eggs

var.egg2 <- p2^2 * (E.f2^2 * s.N2^2 + E.N2^2 * s.f2^2 + s.N2^2 * s.f2^2)
sd.egg2 <- sqrt(var.egg2)
sd.egg2/1e6 # In millions of eggs

var.egg <- var.egg1 + var.egg2
sd.egg <- sqrt(var.egg)
sd.egg/1e6 # In millions of eggs
#--------------------------------------------------------------

### (c) 95% confidence interval, assuming normal distribution:

#---- Enter your code below and briefly annotate as needed ----

  # 95% CI calculated by (E(egg) - 1.96sd, E(egg) + 1.96sd)
cat(round((E.egg - 1.96*sd.egg)/1e6, 2), "to", round((E.egg + 1.96*sd.egg)/1e6, 
                                                      2), "million eggs")

#--------------------------------------------------------------

###############################################################
#### Problem 3:

### (a) Define skewness
  # Skewness (g1) is a discription of assymetry of sample distribution compared 
  # to a normal distribution

### (b) Expected skewness of normally distributed sample
library(moments)
  # The expected skewness of a normal distribution is g1 = 0
  # Justification: since skewness describes deviation in symmetry from a normal 
    # distribution, we would expect that normal distribution would have a 
    # skewness of zero

  # Check answer by repeatedly generating large random samples from a normal 
    # distribution and determining their skewness

skew <- vector() # Create empty vector

for(i in 1:1e4) { # Repeat sampling event 1e4 times
  skew[i] <- skewness(rnorm(1e4)) # Measure skewness from a sample of 1e4
}

mean(skew) # Mean skewness from repeated sampling is approximately 0

### (c) Compute skewness of fecundity measurements from each lake 
skew.E.egg1 <- skewness(f.dat.1$Fecundity) # Skewness of lake one fecundity distribution 

skew.E.egg2 <- skewness(f.dat.2$Fecundity) # Skewness of lake two fecundity distribution 

  # Both lakes have a g1 > 0, indicating that they are right-skewed 
  # Lake two has a g1 closer to 0, indicating that it more closely resembles a 
    # normal distribution
  # Both lakes have a g1 close to 0, indicating they both resemble a normal
    # distribution 

## (d) Bootstraping to estimate uncertainty in skewness
  # Lake one
skew.1 <- vector()

for(i in 1:999) {
  boot <- sample(f.dat.1$Fecundity, replace = T)
  skew.1[i] <- skewness(boot)
}
var(skew.1)
sd(skew.1)

  # 95% CI of skewness for lake one
    # IF we assume the median follows a normal distribution, an approximate 
    # 95% confidence interval is given by +/- 2 sd:
hist(skew.1)
CI1 <- mean(skew.1) + c(-2,2) * sd(skew.1)
CI1
abline(v=CI1, col=2, lwd=2)

    # Or- avoiding the assumption of normality:
CI1q <- quantile(skew.1, c(0.025, 0.975))
abline(v=CI1q, col= 3, lwd=2)

  # Lake two
skew.2 <- vector()

for(i in 1:999) {
  boot <- sample(f.dat.2$Fecundity, replace = T)
  skew.2[i] <- skewness(boot)
}
var(skew.2)
sd(skew.2)

  # 95% CI of skewness for lake two
    # IF we assume the median follows a normal distribution, an approximate 
    # 95% confidence interval is given by +/- 2 sd:
hist(skew.2)
CI2 <- mean(skew.2) + c(-2,2) * sd(skew.2)
abline(v=CI2, col=2, lwd=2)

    # Or- avoiding the assumption of normality:
CI2q <- quantile(skew.2, c(0.025, 0.975))
abline(v=CI2q, col= 3, lwd=2)

### (e)

agostino.test(f.dat.1$Fecundity) # Lake one- 'data have a skewness'

agostino.test(f.dat.2$Fecundity) # Lake two- 'data have a skewness'

  # Both tests agree with our bootstrap estimate of skewness 
  # that the data is right-skewed

# End ####


