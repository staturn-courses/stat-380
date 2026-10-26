


# Case 1: sigma_1 and sigma_2 are known

# Step 1: Set up parameters for population 
sigma_1 <- 3
sigma_2 <- 4

mu1 <- 15
mu2 <- 15

# Step 2: Collect data (randomly draw from the populations)

sample1 <- rnorm(n = 50, mean = mu1, sd = sigma_1)
sample2 <- rnorm(n = 50, mean = mu2, sd = sigma_2)

# Step 3: 95% CI for µ1 - µ2
# xbar1 - xbar2 +/- z_alpha/2*sqrt(sigma12/n1 + sigma22/n2)
xbar1 <- mean(sample1)
xbar2 <- mean(sample2)

xbar1 - xbar2 + c(-1,1)*qnorm(p=1-0.05/2) *
  sqrt(sigma_1^2/length(sample1) + sigma_2^2/length(sample2))
