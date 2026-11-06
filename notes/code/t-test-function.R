set.seed(384)
y <- rnorm(n=30, mean=20, sd=3)

# H0: µ = 20 vs. HA: π ≠ 20

# t-statistic
xbar <- mean(y)
s2 <- var(y)
n <- length(y)

t.stat <- (xbar-20) / sqrt(s2/n)
p.val <- 2*pt(abs(t.stat), df=n-1, lower.tail = F)
ci <- xbar + c(-1,1)*qt(p=1-0.05/2, df=n-1)*sqrt(s2/n)

# Print results
cbind(xbar, t.stat, p.val, lcl=ci[1], ucl=ci[2])

# t.test function
t.test(y, mu = 20, alternative = 'two.sided')


# How close is our rejection rate to the significance level?
nsim <- 10000
reject <- numeric(nsim)
for(i in 1:nsim) {
  y <- rnorm(n=30, mean=20, sd=3)
  test <- t.test(y, mu=20, alternative = 'two.sided')
  p.val <- test$p.value
  reject[i] <- ifelse(p.val < 0.05, 1, 0)
}

mean(reject)

# Two-sample t.test
# H0: µ1 = µ2 vs. HA: µ1 ≠ mu2
y1 <- rnorm(n=30, mean=20, sd=3)
y2 <- rnorm(n=30, mean=20, sd=9)

qqnorm(y1); qqline(y1)
qqnorm(y2); qqline(y2)

sd(y1); sd(y2)

f.stat <- var(y1)/var(y2)
p.val <- 2*min(pf(q=f.stat, df1=length(y1)-1, df2=length(y2)-1),
               pf(q=f.stat, df1=length(y1)-1, df2=length(y2)-1, lower.tail = F))
ci <- f.stat/qf(c(0.975, 0.025), df1=length(y1)-1, df2=length(y2)-1)
cbind(f.stat, p.val, lcl=ci[1], ucl=ci[2])
var.test(y1, y2, alternative = 'two.sided')

t.test(y1, y2, var.equal = F)


# Rejection rate if equal variance assumption is used?
nsim <- 10000
reject.eq <- numeric(nsim)
reject.neq <- numeric(nsim)
for(i in 1:nsim) {
  y1 <- rnorm(n=30, mean=20, sd=3)
  y2 <- rnorm(n=30, mean=20, sd=9)
  test.eq <- t.test(y1, y2, var.equal = T, alternative = 'two.sided')
  test.neq <- t.test(y1, y2, var.equal = F, alternative = 'two.sided')
  p.val.eq <- test.eq$p.value
  p.val.neq <- test.neq$p.value
  
  reject.eq[i] <- ifelse(p.val.eq < 0.05, 1, 0)
  reject.neq[i] <- ifelse(p.val.neq < 0.05, 1, 0)
}
mean(reject.eq)
mean(reject.neq)

plot(1:nsim, cumsum(reject.eq)/1:nsim, type='l', 
     main = 'Rejection Rate of True H0',
     xlab = 'Simulation Number',
     ylab = 'Cumulative mean rejection rate')
lines(1:nsim, cumsum(reject.neq)/1:nsim, type='l', col='red')


