set.seed(380)

# Plot the distributions
curve(dnorm(x, mean=55, sd=5), from = 30, to=75)
curve(dnorm(x, mean=50, sd=5), from=30, to=75, add = T, col='red')

# Try multiple sample sizes
n <- 1:20

# Compute beta for each sample size
# pnorm: get P(Xbar_A ≤ cutoff from Xbar_0)
# qnorm: get cuttoff for rejection region
beta <- pnorm(
  q=qnorm(1-0.05, mean=50, sd=5/sqrt(n)),
  mean=55, sd=5/sqrt(n)
)

# Combine sample size with power
cbind(n, power=1-beta)

# A sample of size 7 produces a power that is at least 0.8
curve(dnorm(x, mean=55, sd=5/sqrt(7)), from = 30, to=75)
curve(dnorm(x, mean=50, sd=5/sqrt(7)), from=30, to=75, add = T, col='red')

# Compute a random sample for data collection
y <- rnorm(n=7, mean=55, sd=5)

# Compute p-value
z <- (mean(y)-50)/(5/sqrt(7))

pnorm(q=z, lower.tail = F)
1-pnorm(q=z)
