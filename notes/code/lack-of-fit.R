# Generate data
set.seed(123)
x <- sample(2:10, size = 100, replace = T)
y <- 2 + x + 0.5*x^2 + rnorm(100, 0, 2)

# Simple plot
plot(x, y)

# Linear model
linear.fit <- lm(y ~ x)
summary(linear.fit)
abline(linear.fit, col = 'red', lwd=3)

# Check residuals
plot(x, resid(linear.fit)); abline(0,0, col='red', lwd=3)
qqnorm(resid(linear.fit)); qqline(resid(linear.fit))


# Test lack of fit
# Option 1: Compare two models with anova() function
linear.lof <- lm(y ~ factor(x))
anova(linear.fit, linear.lof)

# Option 2: Include the quantitative factor as a categorical variable (last term)
anova(lm(y ~ x + factor(x)))

# Include a quadratic term
quad <- lm(y ~ x + I(x^2))
summary(quad)
anova(quad)

# Simple plot
plot(x, y)
xnew <- seq(2,10,l=1000)
yhat <- predict(quad, newdata = list(x=xnew))
lines(xnew,yhat, col = 'red', lty = 'dashed', lwd = 3)

# Check residuals
plot(x, resid(quad)); abline(0,0, col='red', lwd=3)
qqnorm(resid(quad)); qqline(resid(quad))

