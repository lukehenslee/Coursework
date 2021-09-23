#####################################################
#
# FISH 604
# Module 2 (part 3): Continuous distributions

# Franz Mueter
# Last modified: August 3, 2021

#####################################################

# Probability distributions - Continuous distributions

##################### Normal distribution:

## Probability density function
# pdf of N(0, 3^2)
x <- seq(-15, 15, length=100)  # Define range of values for which to compute density
y <-  dnorm(x, mean=0, sd=3)
par(mfrow=c(2,1), mar=c(2,2,1,1))
plot(x,y,type="l", col=4)

# Add other normal pdfs:
lines(x, dnorm(x, 0, 5), col=2)
lines(x, dnorm(x, 0, 8), col=3)
lines(x, dnorm(x, mean=8, sd=3), col=6)

## Cumulative distribution function:
# cdf of N(0, 3^2)
x <- seq(-15, 15, length=100)  # Define range of values for which to compute density
y <-  pnorm(x, mean=0, sd=3)
plot(x,y,type="l", col=4)
abline(h=c(0,1), lty=2)

# Add other normal cdfs:
lines(x, pnorm(x, 0, 5), col=2)
lines(x, pnorm(x, 0, 8), col=3)
lines(x, pnorm(x, mean=8, sd=3), col=6)


## Random number generation:
par(mfrow=c(1,1))
y <- rnorm(n=40, mean=0, sd=3)
# Histogram showing frequency of observations within automatically selected bins:
hist(y)
# Histogram showing density of observations within automatically selected bins
# (i.e. area under histogram sums to 1 like a pdf):
hist(y, prob=T, col=5)
# Add "true" pdf (i.e. pdf from which data were drawn at random:
lines(x, dnorm(x, mean=0, sd=3), col=4, lwd=3)
# Add normal pdf using parameters estimated from data
lines(x, dnorm(x, mean=mean(y), sd=sd(y)), lwd=2, lty=2)
# Add empirical density estimate:
lines(density(y), col="purple", lwd=3)

# Repeat above code several times to see variability in random draws!)
# Repeat with larger n (e.g. 1000)


##################### Uniform distribution:

## Simulate spatially random process 
# (e.g. distribution of flatfish on sandy substrate):

x <- runif(100)
y <- runif(100)
plot(x,y)

# Repeat to see variability:
par(mfrow=c(3,3))
for(i in 1:9) {
 x <- runif(100)
 y <- runif(100)
 plot(x,y)
}

##################### Log-normal distribution:
# Example: Abundances (where present) of jellyfish 
#          in Kodiak trawl samples (n = 36)
jelly <- scan(nlines=2) 
2  79 110  42  40  21 164  82  13 361 205  17  68  49  37  44   7  28   4   2   6  18  40
7   8   2   7  23  12  29   2  93  15  27  40  32

# After log-transformation, the estimated mean abundance 
#   and it's variance are:
(mu <- mean(log(jelly)))
(Var <- var(log(jelly)))


par(mfrow=c(1,1))
hist(log(jelly), prob=T, xlab="", ylab="Probability", 
     main="log-transformed jellyfish abundances", col="slateblue")

x <- seq(0,6,by=0.2)
# Fitted normal distribution:
lines(x, dnorm(x,mean=mu,sd=sqrt(Var)), lwd=2)

hist(jelly, prob=T, xlab="", ylab="Probability", nclass=20, 
     main="'raw' jellyfish abundances", col="slateblue", ylim=c(0,0.035))

x <- c(seq(0,5,by=0.01), 6:400)
# Fitted log-normal distribution:
lines(x, dlnorm(x,mean=mu,sd=sqrt(Var)), lwd=2)

# Estimated mean and standard deviation of abundances:

(exp(mu + Var/2))    # Mean
sqrt((exp(Var) - 1) * exp(2*mu + Var))    # Variance

# Compare to observed variance of the data:
sqrt(var(jelly))



##################### Gamma distribution
# Waiting time until the rth event for a process that occurs randomly at a rate of beta:
x <- seq(1,10, length=100)
y <- dgamma(x, shape=5, rate=2)  # Mean = 5/2 = 2.5
plot(x, y, type="l", col=4)
lines(x, dgamma(x, 5, 1), col=2)  # Mean = 5/1 = 5
lines(x, dgamma(x, 10, 2), col=3)  # Mean = 10/2 = 5

# Cumulative probability density of, e.g. waiting time in hours to observe at least 20 events
# that occur randomly at an average rate of 3 per hour:
y <- pgamma(x, 20,3)
plot(x, y, type="l", col=4)

# What is the waiting time such that we have at least a 95% probability of observing 
# 20 random events occurring at an average rate of 3 per hour:
wt <- qgamma(.95, 20, 3)
abline(h=0.95, v=wt, lty=2)


