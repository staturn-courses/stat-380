# Seed for reproducibility
set.seed(380)

#----- One-sample t-test -----#

# Assume a true population
# mu = 20, sigma = 3

y <- rnorm(n = 30, mean = 20, sd = 3)
y

# Form a hypothesis test
# H0: µ=20 vs. HA: µ≠20

# Calculate t-statistic, p-value, and 95% confidence interval
mu <- 20
ybar <- mean(y)
s2 <- var(y)
n <- length(y)

t.stat <- (ybar-mu)/sqrt(s2/n)
t.stat

p.val <- 2*pt(abs(t.stat), df=length(y)-1, lower.tail = F)

ci <- mean(y) + c(-1,1)*qt(p=1-0.05/2, df = length(y)-1)*sqrt(var(y)/length(y))

# Present results
cbind(ybar = mean(y), t.stat, p.val, lcl=ci[1], ucl=ci[2])

# Use t.test function to get the same output
t.test(y, alternative = 'two.sided', mu = 20)


#----- Simulation for significance level -----#

# Set simulation parameters
nsim <- 100000
reject <- numeric(nsim)

# Use a for loop to take many samples
for(i in 1:nsim) {
  y <- rnorm(n=30, mean=20, sd=3)
  test <- t.test(y, mu=20, alternative = 'two.sided')
  p.val <- test$p.value
  
  # Reject = 1, Fail to Reject = 0
  reject[i] <- ifelse(p.val < 0.05, 1, 0)
}

# Compute rejection rate for true H0
mean(reject)

# Visualize cumulative proportion of rejecting a true H0
plot(x=1:nsim, y=cumsum(reject)/1:nsim, type='l',
     xlab = 'Simulation number', ylab = 'Cumulative rejection rate')
abline(a=0.05, b=0, lty='dashed')


#----- Two-sample t-test -----#
# Assume true populations
# mu = 20, sigma = 3

# Take two samples from independent populations
y1 <- rnorm(n=30, mean=20, sd=3)
y2 <- rnorm(n=30, mean=20, sd=9)

# Descriptive statistics
sd(y1); sd(y2)

# Check normality with Q-Q plots
qqnorm(y1); qqline(y1)
qqnorm(y2); qqline(y2)

# H0: sigma^2_1 = sigma^2_2 vs. HA: sigma^2_1 ≠ sigma^2_2
# Code by hand
f.stat <- var(y1)/var(y2)
p.val <- 2*min(
  pf(f.stat, df1=length(y1)-1, df2=length(y2)-1),
  pf(f.stat, df1=length(y1)-1, df2=length(y2)-1, lower.tail = FALSE)
)
ci <- f.stat/qf(c(0.975, 0.025), df1=length(y1)-1, df2=length(y2)-1)
cbind(f.stat, p.val, lcl=ci[1], ucl=ci[2])

# Built-in function
var.test(y1, y2)

# Through descriptive and inferential statistics, we can assume the variances are unequal

# Form a hypothesis test
# H0: µ1=µ2 vs. HA: µ1≠µ2

# Use unequal variance assumption for t.test function
t.test(y1, y2, alternative = 'two.sided', var.equal = F)

#----- Simulate rejection rate when violating equal variance assumption -----#

# Simulation parameters
nsim <- 100000
reject.eq <- numeric(nsim)
reject.neq <- numeric(nsim)

# Perform many simulations and obtain rejection of H0
for(i in 1:nsim) {
  y1 <- rnorm(n=30, mean=20, sd=3)
  y2 <- rnorm(n=30, mean=20, sd=30)
  
  test.eq <- t.test(y1, y2, var.equal = T)
  test.neq <- t.test(y1, y2, var.equal = F)
  
  p.val.eq <- test.eq$p.value
  p.val.neq <- test.neq$p.value
  
  reject.eq[i] <- ifelse(p.val.eq < 0.05, 1, 0)
  reject.neq[i] <- ifelse(p.val.neq < 0.05, 1, 0)
}

# Compute rejection rates
mean(reject.eq)
mean(reject.neq)

# Visualize the results
plot(1:nsim, cumsum(reject.eq)/1:nsim, type='l',
     main = 'Cumulative rejection rates for two-sample t-test',
     xlab = 'Simulation number',
     ylab = 'Rejection rate')
lines(1:nsim, cumsum(reject.neq)/1:nsim, type='l', col='red')
abline(a=0.05, b=0, lty='dashed')
legend('bottomright',
       legend = c("Student's t-test", "Welch's t-test", 'Significance level = 0.05'),
       col = c('black', 'red', 'black'),
       lty = c(1, 1, 2),
       lwd = c(1, 1, 1))