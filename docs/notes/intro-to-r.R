# Basic Operations
2 + 3
2 - 3
2 * 3
2 / 3
2 ^ 3

# Assigning and using objects
y <- 4
y + 10

# Creating vectors
x <- c(2,5,1,4,5)
x + y

# Some probability functions in R
#   Binomial, gamma, continuous uniform, poisson, normal

# For now, two types of probability functions defined by prefixes
#   - d_: calculates f(x) for pdf/pmf, and P(X=x) for discrete RVs
#   - p_: calculates P(X<=q) for pdf/pmf (this is the cdf)
#   - Note: use ?function_name to get the help file

#--- Distribution examples ---

# Let X ~ binomial(n=10, p=0.5)
# P(X=3) can be found using the following: (10 choose 3) * 0.5^3 * (1-0.5)^(10-3)
dbinom(x=3, size=10, prob=0.5)

# P(X<=3)
pbinom(q=3, size=10, prob=0.5)

# Let X ~ Normal(mu=15, sigma^2=4)
# f(14) is the density of f(x) at x=14, not a probability
dnorm(x=14, mean=15, sd = sqrt(4))

# P(X<=17)
pnorm(q=17, mean=15, sd=2)

# P(13 <= X <= 17)
pnorm(q=17, mean=15, sd=2) - pnorm(q=13, mean=15, sd=2)
# Subtract the area to the left of 13 from the area to the left of 17 to 
# get the area within this interval