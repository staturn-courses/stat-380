# Randomly generate data from N(10, 4)
set.seed(380)
y <- rnorm(n=20, mean=10, sd=2)

# Visualizations
hist(y)
boxplot(y)

# Compute theoretical quantiles
y.sorted <- sort(y)
p <- (1:20 - 0.5) / 20
p

z_p <- qnorm(p=p)

# Create q-q plot
plot(x=z_p, y=y.sorted)

# Using R's built in functions
qqnorm(y)
qqline(y)

# Take a sample from a right-skewed distribution
curve(dgamma(x,2,2), from=0, to=5)
x <- rgamma(n=1000, shape=2, rate=2)
x.sorted <- sort(x)

# Visualize right-skewed data
hist(x)
qqnorm(x); qqline(x)

# Process to line up with with our Gamma distribution
p <- (1:length(x.sorted) - 0.5) / length(x.sorted)
gamma_p <- qgamma(p, 2, 2)
plot(gamma_p, x.sorted,
     main = 'Q-Q Plot for Gamma(2,2)',
     xlab = 'Theoretical Quantiles',
     ylab = 'Sample Quantiles')
