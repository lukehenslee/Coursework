#==================================================================================================
#Project Name: FISH 622 - Final Exam 2021
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 4.15.2021
#
#
#==================================================================================================
#NOTES:

#==================================================================================================
library(tidyverse)
library(dplyr)
library(ggthemes)
library(manipulate)

# Set working directory
setwd("/Users/curryc2/Documents/Students/2021/622 - Quant Fish Pop Dynamics/Content/Final Exam")

# Problem #2 ==================================================================

# Number of ages
Nage <- 15
ages <- 1:Nage

# Dome-shaped Selectivity Parameters:
amax <- 7
delta <- 3

# Fishing Mortality:
Fmort <- 0.2
  
# Natural Mortality
M <- 0.1

# Number of age-1 recruits
Nr <- 1000

# Numbers at age
Na <- vector(length=Nage)

# Catch at age
Ca <- vector(length=Nage)

# SOLUTION:











# Problem #3 ==================================================================

# Size transition matrix
X <- matrix(data=c(0.1, 0,   0,   0,   0,   0,   0,
                   0.6, 0.3, 0,   0,   0,   0,   0,
                   0.3, 0.5, 0.5, 0,   0,   0,   0,
                   0,   0.2, 0.3, 0.7, 0,   0,   0, 
                   0,   0,   0.2, 0.2, 0.6, 0,   0,
                   0,   0,   0,   0.1, 0.2, 0.6,   0,
                   0,   0,   0,   0,   0.2, 0.4 ,  1),
nrow=7, ncol=7, byrow=TRUE)

# Check columns sum to 1
apply(X, 2, sum)

# SOLUTION:











# Problem #4 ==================================================================

# Initial numbers at size at time t=0
Nstart <- c(1000, 800, 600, 400, 200, 100, 10)

# Simulate numbers-at-age across 25 years.

# SOLUTION: 









# Update to account for fishing mortality

# SOLUTION: 








# Problem #5 ==================================================================
dat <- read.csv("EBS_turbot_specimen.csv",  header=TRUE)

dat <- dat %>% filter(!is.na(Length..mm.), !is.na(Age..years.))

ggplot(data=dat, aes(x=Age..years., y=Length..mm., color=Sex)) +
      geom_point()

head(dat)

str(dat)

# SOLUTION:






