################################################################################

# Mod 2 (3) lecture exercises- reviewed and worked through on 9/1 and 9/2/2021

# Luke Henslee

################################################################################

# Continuous distributions: Slide 21 Exercise ##################################
mu <- 3.1
Var <- 1.79

exp(mu + Var/2) # Mean of untransformed data

(exp(Var)-1) * exp(2*mu + Var) # Variance of untransformed data


# Continuous distributions: PDF of Normal distribution #########################
# Repeat with larger n (e.g. 1000)
x <- seq(-15, 15, length=100)
## Random number generation:
par(mfrow=c(1,1))
y <- rnorm(n=1000, mean=0, sd=3)
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

  # Probability distribution more closely resembles normal with larger sample 
  # size. 

# Continuous distributions: Gamma #############################################
# What is the waiting time such that we have at least a 95% probability of observing 
# 20 random events occurring at an average rate of 3 per hour:
wt <- qgamma(.95, 20, 3)
abline(h=0.95, v=wt, lty=2)

cat("The waiting time for a 95% probability of observing 20 random events 
    occurring at 3 per hour is", wt)


