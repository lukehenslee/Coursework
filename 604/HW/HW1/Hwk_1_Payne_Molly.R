##########################################################

# FISH 604
# Homework 1 script file
#Molly K. Payne

# Expected values and variances: Sockeye salmon fecundity

# Franz Mueter - last updated Sep 5, 2019

##########################################################


#### Problem 1:

### Import data:
f.dat <- read.csv("sockeye fecundity.csv", head=T)
str(f.dat)           # Examine data
table(f.dat$Lake)    # Number of observations by lake

### (a) Plot histograms and boxplots

#---- Enter your code below and briefly annotate as needed ----
#Histograms:
f.dat$Lake <- as.factor(f.dat$Lake)
hist(f.dat$Fecundity[f.dat$Lake == "Lake1"], main = "Lake #1 Fecundity")
#slightly right-skewed, most data points occurring at 3000 (eggs)
hist(f.dat$Fecundity[f.dat$Lake == "Lake2"], main = "Lake #2 Fecundity")
#more normally distributed than lake 1, most data points occurring at 3500 (eggs)

#Boxplots:
boxplot(f.dat$Fecundity[f.dat$Lake == "Lake1"], main = "Lake #1 Fecundity")
#there are several outliers above the boxplot maximum of ~5000
boxplot(f.dat$Fecundity[f.dat$Lake == "Lake2"], main = "Lake #2 Fecundity")
#there are several outliers above the boxplot maximum of ~5500
#--------------------------------------------------------------

# For further examining the data, it is useful to split the 
# fecundity vector into two components ('split' creates a list)
(f.dat2 <- split(f.dat$Fecundity, f.dat$Lake))


### (b) Compute mean fecundities (expected values) and their standard errors:

#---- Enter your code below and briefly annotate as needed ----
#Lake 1
sum(is.na(f.dat2$Lake1)) #no missing values
mean(f.dat2$Lake1)
#standard error (of the mean):
(sd(f.dat2$Lake1))/(sqrt(length(f.dat2$Lake1)))
#another way to check:
plotrix::std.error(f.dat2$Lake1)

#Lake 2
sum(is.na(f.dat2$Lake2)) #no missing values
mean(f.dat2$Lake2)
#standard error (of the mean):
(sd(f.dat2$Lake2))/(sqrt(length(f.dat2$Lake2)))
#another way to check:
plotrix::std.error(f.dat2$Lake2)
#--------------------------------------------------------------


### (c) Compare means 
# Check out help files for 't.test' and 'oneway.test'
?t.test #assumes unequal variances unless you specify var.equal = TRUE
?oneway.test #also assumes unequal variances

#---- Enter your code below and briefly annotate as needed ----
t.test(f.dat2$Lake1, f.dat2$Lake2)
oneway.test(Fecundity ~ Lake, data = f.dat)
#The difference in mean fecundities in lake 1 and lake 2 is not equal to 0. Said
#another way, the mean fecundities of female spawners between the two lakes are
#significantly different from one another based on the results of the t-test and
#the one way-test (p < 2e-16). Therefore I would report separate estimates of
#fecundity and associated standard errors to my supervisors because this suggests
#to me that different factors in the two lakes are producing different fecundities.
#The two lakes should thus not be analyzed as one system
#--------------------------------------------------------------

###############################################################
#### Problem 2:

### Input values needed for expected values and variance:

# Proportion of females by lake:
p1 <- 0.58
p2 <- 0.52

# Expected numbers of total spawners:
E.N1 <- 124360
E.N2 <- 73608

# Standard errors:
s.N1 <- 23245
s.N2 <- 10120

# Other quantities from 1b above
E.f1 <- mean(f.dat2$Lake1)
s.f1 <- (sd(f.dat2$Lake1))/(sqrt(length(f.dat2$Lake1)))

E.f2 <- mean(f.dat2$Lake2)
s.f2 <- (sd(f.dat2$Lake2))/(sqrt(length(f.dat2$Lake2)))


### (a) Expected number of eggs (in millions):
# Note that pi is a constant, and Ni and fi are independent

#---- Enter your code below and briefly annotate as needed ----
#for lake 1
(p1*E.N1*E.f1)

#for lake 2
(p2*E.N2*E.f2)

#Total for both lakes
E_egg <- (p1*E.N1*E.f1) + (p2*E.N2*E.f2)
#--------------------------------------------------------------



###  (b) Variance:

#---- Enter your code below and briefly annotate as needed ----
#Variance times a constant (pi)
#var(Eggs_i) = var(pi*Ni*fi) = pi^2 * var(Ni*fi)
#Equation for variance of product of two independent variables * pi (the constant):
#            = pi^2 * ((mu_fi*var(Ni)) + (mu_Ni*var(fi)) + (var(Ni)*var(fi)))
#Lake 1 
var1 <- (p1^2) * ((E.f1*(s.N1^2)) + (E.N1*(s.f1^2)) + ((s.N1^2)*(s.f1^2)))
#USE s.f1 OR var(f.dat$Lake1) FOR THE VARIANCE OF FECUNDITY?

#Lake 2
var2 <- (p2^2) * ((E.f2*(s.N2^2)) + (E.N2*(s.f2^2)) + ((s.N2^2)*(s.f2^2)))

#Variance of total egg production:
var_egg <- var1 + var2
#--------------------------------------------------------------



### (c) 95% confidence interval, assuming normal distribution:

#---- Enter your code below and briefly annotate as needed ----
#CI = (mu - 1.96*sd, mu + 1.96*sd)
sd_egg <- sqrt(var_egg)
CI_egg <- c((E_egg - (1.96*sd_egg)), (E_egg + (1.96*sd_egg)))
#--------------------------------------------------------------



###############################################################
#### Problem 3

### (a) Define skewness:
#Skewness = measure of how a sample differs in shape from a symmetrical, or nor-
#mal distribution. If skewness (g) is > 0, the distribution is right-skewed, if
#it is < 0, it is left-skewed, and if it = 0, it is a normal distribution


### (b) What is the expected skewness of a sample from a normally distributed 
#populations?
#The expected skewness of a sample from a normally distributed population is 0, 
#or should be approximately 0 because skewness measures deviation away from that
#distribution
install.packages('moments')
library(moments)
?skewness
skewness(rnorm(10000))

g <- vector(length = 1000)
for (i in 1:length(g)) {
  g[i] <- skewness(rnorm(10000))
}
mean(g) #0.0006133 ~= 0


### (c) Compute skewness for the two lakes' fecundity measurements
skewness(f.dat2$Lake1) #0.96
skewness(f.dat2$Lake2) #0.38
#possibly some evidence of (right) skewness since g > 0, more so for lake 1.
#Looking at the histogram, you can see some right-skew:
hist(f.dat2$Lake1)


### (d) Bootstrap
#Lake 1 ________________________________________________________________________
B <- 1000
res <- rep(NA, B)
for(i in 1:B) res[i] <- skewness(sample(f.dat2$Lake1, replace = T))
res
var(res) #variance quantifies uncertainty

#Construct 95% confidence interval
sd(res)
CI1 <- mean(res) + c(-2,2) * sd(res)
CI1 #does not contain 0, therefore the fecundity estimates of lake 1 do not follow
#a normal distribution (evidence of right-skew)

#Lake 2 ________________________________________________________________________
res2 <- rep(NA, B)
for(i in 1:B) res2[i] <- skewness(sample(f.dat2$Lake2, replace = T))
res2
var(res2) #variance quantifies uncertainty

#Construct 95% confidence interval
sd(res2)
CI1 <- mean(res2) + c(-2,2) * sd(res2)
CI1 #also does not contain 0, therefore the fecundity estimates of lake 1 do not
#follow a normal distribution. There is evidence of right-skew here, though less
#than in lake 1

### (e) Agostino test 
?agostino.test
agostino.test(f.dat2$Lake1) #reject null, lake 1 fecundity data does have skew,
#estimated value is 0.96
mean(res) #which is very close to what the bootstrap distribution estimated

agostino.test(f.dat2$Lake2) #reject null, lake 2 fecundity data does have skew,
#estimated value is 0.38
mean(res2) #also very close to estimate from bootstrap distribution
