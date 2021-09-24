##########################################################

# FISH 604
# Homework 1 script file (template)

# Expected values and variances: Sockeye salmon fecundity

# Kevin Siwicke - 8/28/21 
# Franz's HW 1 template was used

##########################################################

#### Problem 1:

### Import data:
f.dat <- read.csv("sockeye fecundity.csv", head=T)
str(f.dat)           # Examine data
table(f.dat$Lake)    # Number of observations by lake

### (a) Plot histograms and boxplots
library(ggplot2)
ggplot(f.dat, aes(x=Fecundity, fill=Lake)) + 
  geom_histogram(col="black", alpha=0.7, position="identity") +
  scale_fill_manual(values=c("red", "blue")) +
  theme_bw()

# This boxplot shows median, hinges at 25/75 quartiles, and whiskers at 
# largest value no more than 1.5 Inter quartile range, and pts beyond whiskers shown
ggplot(f.dat, aes(x=Lake, y=Fecundity, fill=Lake)) + 
  geom_boxplot(col="black") + 
  scale_fill_manual(values=c("red", "blue")) +
  theme_bw()
 
# NOTES: Sockeye in Lake 2 appear to have a slightly higher fecundity than Lake 1
# Fecundity seems close to normally distributed in both lakes, though slight right tails exist 

#--------------------------------------------------------------

# For further examining the data, it is useful to split the 
# fecundity vector into two components ('split' creates a list)
(f.dat2 <- split(f.dat$Fecundity, f.dat$Lake))

### (b) Compute mean fecundities (expected values) and their standard errors:

# If we assume these are normal distributions, then the E(x) = y_bar (i.e., arithmetic mean)
Ef = c(mean(f.dat2$Lake1), mean(f.dat2$Lake2))
sf = c(sd(f.dat2$Lake1), sd(f.dat2$Lake2))

# Confirm that sd() is the same as formula for unbiased estimator. It is!
# sqrt(sum((f.dat2$Lake1-mean(f.dat2$Lake1))^2)/(length(f.dat2$Lake1)-1))

#--------------------------------------------------------------

### (c) Compare means 
# Check out help files for 't.test' and 'oneway.test'

t.test(f.dat2$Lake1, f.dat2$Lake2, var.equal=FALSE) # defaults to a two-sided test

# The t-test rejects the null hypothesis that the difference in means is 0 (p << 0.05)
# and the estimated 95% CI of the difference is that Lake 2 has a fecundity greater than
# Lake 1 by 444 to 694.8

oneway.test(Fecundity~Lake,data=f.dat, var.equal=FALSE) # Also rejects null

# Look at overall mean and standard deviation
mean(f.dat$Fecundity)
sd(f.dat$Fecundity)

# NOTES: I would use separate estimates of fecundity (means and errors) by lakes as the t.test 
# shows the means are significantly different, and the error is actually smaller when each is 
# considered separately (719/734) compared to 780 when considered together.

#--------------------------------------------------------------

###############################################################
#### Problem 2:

### Input values needed for expected values and variance:

# Given data:
N = c(124360, 73608) # Expected numbers of total spawners
sN = c(23245, 10120) # Standard errors of number of spawners
p = c(.58, .52) # Proportion of females by lake

# Expeced fecundity and errors from above, (Ef and sf)
  
### (a) Expected number of eggs (in millions):
# Note that pi is a constant, and Ni and fi are independent

#---- Enter your code below and briefly annotate as needed ----
# Let G be the number of eggs, then the expectation is 
# E(Gi) = E(Ni*pi*fi)
#       = pi*E(Ni*fi) # because pi is a constant
#       = pi*E(Ni)*E(fi), which we can do because they are independent
#       = pi*Ni*fi
# AND E(G1 + G2) = E(G1) + E(G2)

G = N * p * Ef # Lake 1 = 244,416,196, and Lake 2 = 151,497,945

Tot = G[1] + G[2] 

# Point estimate is ~ 395.9 million fish in Lake1 and Lake2.

#--------------------------------------------------------------

###  (b) Variance:

# var(Gi) = var(Ni*fi) # There is no error with the probabilities, so don't need pi
#         = E(Ni)^2 * var(fi) + E(fi)^2 * var(Ni) + var(Ni)*var(fi)              

# Variance by lake
varG = p^2 * (N^2 * sf^2 + Ef^2 * sN^2 + sf^2 * sN^2)

# Standard deviations by lake:
sG = sqrt(varG)

# Standard deviation of the total number of eggs
sTot <- sqrt(sum(varG))

### (c) 95% confidence interval, assuming normal distribution:
low = round((Tot - 1.96*sTot)/1000000)
up = round((Tot + 1.96*sTot)/1000000)

print(paste("The 95% CI is from", low, "to", up, "million eggs."))

#--------------------------------------------------------------

###############################################################
#### Problem 3:

### (a) Define skewness.
# Skewness is a measure of how the shape of a distribution differs from a symmetrical one.

#--------------------------------------------------------------

### (b) what is the expected skewness (average across many samples) of a sample from a 
# normally distributed population?

# The expected value of skewness for a normal distribtuion is 0 because a truly normal distribution
# is perfectly symmetric, thus no tails or no skewness.
library(moments)
skewEx = (1:1000)
for( i in 1:1000){
  normEx = rnorm(100)
  skewEx[i] = skewness(normEx)
}  

mean(skewEx) # very close to 0
hist(skewEx) # ~normal distribution with mean ~ 0

#--------------------------------------------------------------

### (c) Compute the skewness for the two sets of fecundity measurements in lake 1 and lake 2 (separately)
# and compare qualitatively. Is there any evidence of skewness? 
skewness(f.dat2$Lake1) # = 0.961
skewness(f.dat2$Lake2) # = 0.382

# There is evidence that both are right-skewed, though Lake 1 is almost 3 times greater.

#--------------------------------------------------------------

# (d) Follow the bootstrap example to estimate the uncertainty in the skewness measure for each of 
# the two lakes and construct 95% confidence intervals based on the 2.5th and 97.5th percentiles 
# of the bootstrap distribution. 
B=999
skew1 <- rep(NA, B)
skew2 <- rep(NA, B)

for(i in 1:B) {
  skew1[i] <- skewness(sample(f.dat2$Lake1, replace = T))
  skew2[i] <- skewness(sample(f.dat2$Lake2, replace = T))
}
  
hist(skew1)
mean(skew1)
sd(skew1)
CI1 = quantile(skew1, c(0.025, 0.975))
abline(v=CI1, col=4, lwd=2)
# NO, the 95% CI for skewness in Lake 1 does not include 0, the expected value of skewness for a normal dist.

hist(skew2)
mean(skew2)  # Bootstrap estimate of median
sd(skew2)

CI2 = quantile(skew2, c(0.025, 0.975))
abline(v=CI2, col=4, lwd=2)
# NO, the 95% CI for skewness in Lake 2 does not include 0, the expected value of skewness for a normal dist.

# Does the 95% confidence interval include the expected value for a normal distribution? NO
  
# What can you conclude about skewness of fecundity in the two lakes? 
# At a 95% confidence level, I conclude that the skewness is not 0 for either lake.

#--------------------------------------------------------------

### (e) Finally, use the 'agostino.test()' (package: moments) to test for skewness. 
# Compare conclusions to those in (d) (5 pts)

agostino.test(f.dat2$Lake1) # rejects the null of no skewness for Lake 1 (p<<<0.05)

agostino.test(f.dat2$Lake2) # rejects the null of no skewness for Lake 2 (p=0.013)

# The D'Agostino skewness test rejects the null hypothesis of no skewness for both lakes,
# which agrees with the conclusions above, that both are right-skewed, and Lake 1 to greater degree

#--------------------------------------------------------------

