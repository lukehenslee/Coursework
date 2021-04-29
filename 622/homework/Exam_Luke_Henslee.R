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
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Coursework/622/data")

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

# 1) Dome-shaped selectivity 
dome <- function(age, amax, delta) {
  p <- 0.5*(sqrt(amax^2 + 4*delta^2) - amax )
  sa <- ((age/amax)^(amax/p)) * exp((amax-age)/p)
  
  return(sa)
}

sa <- dome(ages, amax, delta)

sa

# 2) Fishing mortality at age
Fa.fun <- function(Fmort, sa) {
  Fma <- Fmort * sa
  return(Fma)
}

Fa <- Fa.fun(Fmort, sa)

Fa

# 3) Total instantaneous mortality at age
Z.fun <- function(Fmort, sa, M) {
  Fma <- Fa.fun(Fmort, sa)
  Za <- Fma + M
  return(Za)
}

Z <- Z.fun(Fmort, sa, M)

Z

# 4) Numbers at age
Na[1] <- Nr

for(i in 2:length(Na)) {
  Na[i] <- Nr * exp(-Z[i] * (ages[i] - ages[1]))
}

Na

# 5) Catch at age
for(i in 1:length(Na)) {
  Ca[i] <- Nr * (Fa[i]/Z[i]) * (1 - exp(-Z[i] * (ages[i] - ages[1])))
}

Ca

# Creat dataframe to export for excel plots
write.csv(data.frame(cbind(Z, Na, Ca)), "Exam_problem_2.csv")


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

# 1) Create survival matrix (S)
S <- matrix(data = c(0.3, 0, 0, 0, 0, 0, 0,
                     0, 0.4, 0, 0, 0, 0, 0,
                     0, 0, 0.5, 0, 0, 0, 0,
                     0, 0, 0, 0.7, 0, 0, 0,
                     0, 0, 0, 0, 0.7, 0, 0,
                     0, 0, 0, 0, 0, 0.8, 0,
                     0, 0, 0, 0, 0, 0, 0.8),
            nrow = 7, ncol = 7, byrow = T)

# 2) Creat recruitment matrix (R)
S0 <- 0.04

R <- matrix(data = c(0, 0, 0, 50, 100, 150, 200,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0),
            nrow = 7, ncol = 7, byrow = T)

R[1,] <- R[1,] * S0

R

# Problem #4 ==================================================================

# Initial numbers at size at time t=0
Nstart <- c(1000, 800, 600, 400, 200, 100, 10)

# Simulate numbers-at-age across 25 years.

# SOLUTION: 

# 1) Create matrix N
N <- matrix(data = NA, nrow = 7, ncol = 25, dimnames=list(paste("Class", c(1:7)), paste("Year", c(1:25))))

# 2) Simulate
for(i in 0:(ncol(N) - 1)) {
  if(i == 0) {
    N[,i + 1] <- (X %*% S + R) %*% Nstart
  } else {
    N[,i + 1] <- (X %*% S + R) %*% N[,i]
  }
}

# 3) Plot 
barplot(N, las=2, ylab="Numbers", col=cm.colors(n=7), legend = rownames(N), 
        args.legend = list(x = "bottomright"))

# Update to account for fishing mortality

# SOLUTION: 

# 1) Update survival matrix (S)
  # Vector of size-selective fishing mortalities
si <- c(0, 0, 0, 0.2, 0.5, 0.75, 1)

F <- 0.2

siF <- vector(length = length(si))

for(i in 1:length(si)) {
  siF[i] <- si[i] * F
}

S2 <- S * exp(-siF)

# 2) Create matrix N2
N2 <- matrix(data = NA, nrow = 7, ncol = 25, dimnames=list(paste("Class", c(1:7)), paste("Year", c(1:25))))

# 3) Simulate
for(i in 0:(ncol(N2) - 1)) {
  if(i == 0) {
    N2[,i + 1] <- (X %*% S2 + R) %*% Nstart
  } else {
    N2[,i + 1] <- (X %*% S2 + R) %*% N2[,i]
  }
}

# 4) Plot 
barplot(N2, las=2, ylab="Numbers", col=cm.colors(n=7), legend = rownames(N), 
        args.legend = list(x = "topright"))

# Problem #5 ==================================================================
dat <- read.csv("EBS_turbot_specimen.csv",  header=TRUE)

dat <- dat %>% filter(!is.na(Length..mm.), !is.na(Age..years.))

ggplot(data=dat, aes(x=Age..years., y=Length..mm., color=Sex)) +
      geom_point()

head(dat)

str(dat)

# SOLUTION:






