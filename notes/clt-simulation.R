# Seed for reproducibility
set.seed(380)

# Let X~N(0,1)

# Simple graph of plot
# Here, the curve function evaluates an expression written with x 
# dnorm: f(x) for N(0,1)
curve(dnorm(x), from = -5, to = 5)

# Sample n=30 from this distribution
x <- rnorm(n=30, mean=0, sd=1)

# Create a histogram of our responses
hist(x)

# Take many samples using the replicate function
# n: number of samples to take
# rnorm(n): number of individuals for one sample
many_samples <- replicate(n=1000, rnorm(n=30))

# Show the dimensions of many_samples: 30 rows and 1000 columns
dim(many_samples)

# Obtain the means for each sample with the apply function
# X: an array
# MARGIN: whether to use rows (1) or columns(2)
#   note: we want means for each sample, so means of the columns
# FUN: function to apply
many_means <- apply(many_samples, MARGIN=2, FUN=mean)
hist(many_means)

# From CLT:
#  - E(Xbar)=0
#  - V(Xbar)=1/30=0.033
#  - SD(Xbar)=sqrt(1/30)=0.183
mean(many_means)
var(many_means)
sd(many_means)

# Consider a non-normal distribution
# X ~ Gamma(alpha=2, beta=2)
#  - E(X)=2*2 = 4
#  - V(X)=2*2^2 = 8
curve(dgamma(x, shape=2, scale=2), from=0, to=20)

# Take many samples of size 20
many_samples_gamma <- replicate(n=1000, rgamma(n=20, shape=2, scale=2))
many_means_gamma <- apply(many_samples_gamma, 2, mean)
hist(many_means_gamma)

# E(Xbar)=4
# V(Xbar)=8/20=0.4
# SD(Xbar)=sqrt(0.4)=0.6324555
mean(many_means_gamma)
var(many_means_gamma)
sd(many_means_gamma)

# Your turn: Try this process for a Poisson Distribution with n=4 and n=30
# What happens when a large sample size is not taken?
# X ~ Poisson(lambda=1)
# - dpois(lambda)
# - rpois(lambda)

lambda <- 1
x <- 0:15
fx <- dpois(x, lambda=lambda)
barplot(fx, names.arg = x,
        main = 'Poisson Distribution',
        xlab = 'X', ylab = 'P(X=x)')
