##########################################################
  
# Homework 1 script file with solutions
# Expected values and variances: Sockeye salmon fecundity

##########################################################


# This shows one approach to examining the fecundity data for 
# significant differences between groups:


############################ Problem 1:

### Import data:
f.dat <- read.csv("sockeye fecundity.csv", head=T, as.is=F)
# 'as.is' ensures that 'Lake' is imported as factor
str(f.dat)           # Examine data
table(f.dat$Lake)    # Number of observations

### (a) Plot histograms and boxplots
library(lattice)
histogram(~ Fecundity | Lake, f.dat, layout=c(1,2))
plot(Fecundity ~ Lake, data=f.dat)

# Or, using ggplot:
library(ggplot2)
ggplot(f.dat, aes(x = Fecundity, fill = Lake)) + 
  geom_histogram(position = position_dodge())
ggplot(f.dat, aes(x = Lake, y = Fecundity, fill = Lake)) + 
  geom_boxplot()

# Both the histograms and boxplots suggest a fairly substantial
# difference in fecundity between the two lakes

# For further examining the data, it is useful to split the 
# fecundity vector into two components ('split' creates a list)
f.dat2 <- split(f.dat$Fecundity, f.dat$Lake)
f.dat2

### (b) Compute mean fecundities (expected values) and their standard errors:
(E.f1 <- mean(f.dat2$Lake1))
(E.f2 <- mean(f.dat2$Lake2))

(n1 <- length(f.dat2$Lake1))
(s.f1 <- sd(f.dat2$Lake1) / sqrt(n1))

(n2 <- length(f.dat2$Lake2))
(s.f2 <- sd(f.dat2$Lake2) / sqrt(n2))


### (c) Compare means:
t.test(f.dat2$Lake1, f.dat2$Lake2)

# OR, equivalently (these tests are identical - note that
# the F-statistic for this simple model is simply equal 
# to the square of the t-statistic):
oneway.test(Fecundity ~ Lake, data=f.dat, var.equal=F)

# The default for both of these functions is to NOT assume equal variances
# by group. Although the variances / standard errors are quite similar 
# between the two lakes, it may be prudent to not rely on that assumption

# We can conclude that there is a highly significant difference in mean 
# fecundity between the two lakes as it is highly unlikely to get a t-value
# or F-value of the observed magnitude by chance alone!

# As an aside, you can formally test for equality of variances using a 
# 'Levene's test' as, for example, implemented in the 'car' library:
library(car)
leveneTest(Fecundity ~ Lake, data=f.dat)
# This does not suggest a significant difference in variance between
# lakes, hence it would be OK to run a pooled t.test or oneway test:
oneway.test(Fecundity ~ Lake, data=f.dat, var.equal=T)
# Note that the degrees of freedom change because we no longer 
# estimate separate variances, requiring two less parameters


############################ Problem 2:

## Input values needed for expected values and variance:
# Proportion of females by lake:
p1 <- 0.58; p2 <- 0.52
# Expected numbers of total spawners:
E.N1 <- 124360; E.N2 <- 73608
# Standard errors:
s.N1 <- 23245; s.N2 <- 10120

# Other quantities defined above!
  
###  (a) Expected value: 
#     E(E1 + E2) = E(E1) + E(E2)
#                = E(p1 * N1 * f1) + E(p2 * N2 * f2)
#                = p1 * E(N1) * E(f1) + p2 * E(N2) * E(f2)
# Note that pi is a constant, hence E(pi*Ni*fi) = pi * E(Ni*fi)
# and Ni and fi are independent, hence E(Ni * fi) = E(Ni) * E(fi)

## Expected number of eggs (in millions):
(E.Eggs <- (p1 * E.N1 * E.f1 + p2 * E.N2 * E.f2) / 10^6)
#   = 396 million eggs


###  (b) Variance:

#   var(E1 + E2) = var(E1) + var(E2)
#                = var(p1 * N1 * f1) + var(p2 * N2 * f2)
#                = p1^2 * var(N1 * f1) + p2^2 * var(N2 * f2)

# where:

#   var(N1 * f1) = E(f1)^2 * var(N1) + E(N1)^2 * var(f1) + var(N1) * var(f1)
#   var(N2 * f2) = E(f2)^2 * var(N2) + E(N2)^2 * var(f2) + var(N2) * var(f2)

# Compute variance:
varN1f1 <- E.f1^2 * s.N1^2 + E.N1^2 * s.f1^2 + s.N1^2 * s.f1^2
varN2f2 <- E.f2^2 * s.N2^2 + E.N2^2 * s.f2^2 + s.N2^2 * s.f2^2

var.Eggs <- p1^2 * varN1f1 + p2^2 * varN2f2
(se.Eggs <- sqrt(var.Eggs)/10^6)  # in millions

### (c) 95% confidence interval, assuming normal distribution:
round(E.Eggs + c(-1.96, 1.96) * se.Eggs)


var_E1 <- p1^2*(E.f1^2*(s.N1*sqrt(E.N1))^2+(E.N1^2)*(sd(f.dat2$Lake1))^2+((s.N1*sqrt(E.N1))^2)*(sd(f.dat2$Lake1))^2)
var_E2 <- p2^2*((E.f2^2)*(s.N2*sqrt(E.N2))^2+(E.N2^2)*(sd(f.dat2$Lake2))^2+((s.N2*sqrt(E.N2))^2)*(sd(f.dat2$Lake2))^2)
var_total <- var_E1 + var_E2

###############################################################
#### Problem 3:
## Construct 95% confidence for skewness

## (a) Skewness (definition): 
# Skewness is a measure of asymmetry in a distribution, whereby 
# the values of random variable are skewed right if there are
# more extreme values in the right tail of the distribution, such
# that most of the observations are smaller than the mean and the 
# mean is larger than the median (and the mode) of the distribution
# The opposite applies to left-skewed distributions

## (b) Expected skewness for sample from a normal distribution?
# The expected skewness for a normal distribution is 0. Right-skewed
# distributions have skewness > 0, left-skewed distributions have
# skewness < 0. 

# This is obvious from the formula as the expectation for Y.i - Y.bar
# is zero for a symmetrical distribution and larger than zero for 
# distributions with an excess of large values in the right tail
# (Y.i - Y.bar > 0 on average)

# Let's examine skewness for random draws from a normal distribution
# with 250 observations (similar to number of fecundity measurements):

# Here is the expected distribution of skewness for such samples 
hist(xbar <- replicate(1000, skewness(rnorm(250))))
abline(v=mean(xbar), col=2, lwd=3)
# On average, the skewness is very close to zero, as expected

## (c) Skewness of fecundity:

# Lake 1:
skewness(f.dat2$Lake1)

# Lake 2:
skewness(f.dat2$Lake2)

# Both lakes have a positively skewed (right-skewed) distribution, but the 
# skew is much larger for lake 1. Comparing the value for lake 2 to the simulated 
# range of skewness for a sample of N=250 above suggests that skewness may be 
# significantly different from 0 as the value is in the far tail of the simulated
# values for a normal distribution. 

# We'll formally test this using both a bootstrap (d) and, 
# for comparison, a parametric test (e):

## (d) Bootstrap confidence intervals for skewness in fecundity by lake:
B <- 999  # Number of bootstrap replicates

# Lake 1:
out1 <- numeric(B)  # bootstrap output vector for Lake 1
for(i in 1:B) {
  boot1 <- sample(f.dat2$Lake1, replace=T)
  out1[i] <- skewness(boot1) 
}
par(mfrow=c(2,1))
hist(out1)
mean(out1)  # Bootstrap estimate of skewness
sd(out1)^2    # Bootstrap estimate of variance of skewness for Lake 1

# We can construct a number of different confidence intervals using the
# bootstrap results. I will illustrate two possibilities:

# 1. 95% normal confidence interval (assuming normal distribution for skewness,
#    which seems reasonable based on the histogram):
mean(out1) + c(-1.96, 1.96) * sd(out1)

# 2. Percentile based confidence interval based on the 2.5th and 97.5th 
#    percentiles of the bootstrap distribution:
quantile(out1, c(0.025, 0.975))

# The two confidence intervals are very similar and neither includes 0

# Clearly, any confidence interval for skewness of fecundity in lake 1
# does not include zero, hence fecundity is signficantly right-skewed

# This 'confidence interval test' could be used formally to assess
# the significance of the observed skewness. A 'conservative' p-value 
# could be computed based on the number of bootstrapped values 
# less than 0 (which is zero in this case):
(sum(out1<0) + 1) / (B+1)
# Hence skewness for fecundity in lake 1 is different from 0 at
# a significance level of p < 0.001

# Lake 2:
out2 <- numeric(B)  # bootstrap output vector for Lake 1
for(i in 1:B) {
  boot2 <- sample(f.dat2$Lake2, replace=T)
  out2[i] <- skewness(boot2) 
}
hist(out2)
mean(out2)  # Bootstrap estimate of skewness
sd(out2)^2    # Bootstrap estimate of variance of skewness for Lake 2

# 1. 95% normal confidence interval:
mean(out2) + c(-1.96, 1.96) * sd(out2)

# 2. Percentile based confidence interval:
quantile(out2, c(0.025, 0.975))

# Significance of skewness:
(sum(out2<0) + 1) / (B+1)
# Skewness for fecundity in lake 2 is different from 0 at a significance
# level of p < 0.01 (when I ran it repeatedly - results may vary!)

## (e) D'Agostino's test of skewness
agostino.test(f.dat2$Lake1)
agostino.test(f.dat2$Lake2)

# The Agostino test confirms the bootstrap results and the null hypothesis
# that skewness in each of the lakes is zero is rejected in both cases

