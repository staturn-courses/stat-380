# Set seed for reproducibility
set.seed(380)

# Simulate values
n <- 20
n_trt <- 4
y <- rnorm(n*n_trt, mean = 30, sd = 5) |> round(1)
trt <- rep(letters[1:n_trt], each = n)
df <- data.frame(trt, y)
write.csv(df, 'notes/data/one-way-anova-data.csv', row.names = F)


# Read in data
my_data <- read.csv('notes/data/one-way-anova-data.csv') #Change file path to where you have the data stored 
head(my_data)

# Install packages
install.packages(c('tidyverse', 'emmeans'))

# Load dplyr package
library(dplyr)

# Calculate overall and group means
my_data2 <- my_data %>% 
  mutate(ybar=mean(y)) %>% 
  group_by(trt) %>% 
  mutate(ybar_i=mean(y),
         n_i=n())

# Compute sums of squares
ssa <- with(my_data2, sum((ybar_i-ybar)^2))
sse <- with(my_data2, sum((y-ybar_i)^2))
sst <- with(my_data2, sum((y-ybar)^2))

ssa; sse; ssa+sse; sst

# Compute mean square
num_trt <- length(unique(my_data2$trt))
df_a <- num_trt-1
df_e <- nrow(my_data2)-num_trt
df_t <- nrow(my_data2)-1
msa <- ssa/df_a
mse <- sse/df_e

# F
f <- msa/mse

# p-value
p.val <- pf(f, df_a, df_e, lower.tail = F)

# Arrange results
data.frame(Source=c('Trt', 'Error', 'Total'),
      DF = c(df_a, df_e, df_t),
      SS = c(ssa, sse, sst),
      MS = c(msa, mse, NA),
      f.stat = c(f, NA, NA),
      p.val = c(p.val, NA, NA))



# Using R to fit the model
fit <- aov(y ~ trt, data = my_data)
anova(fit)

# Contrasts
# H0: (µ1+µ2) = (µ3+µ4)
# install.packages("emmeans")
library(emmeans)
emm <- emmeans(fit, specs=~trt)
emm

contrast(emm, list(
  '(µ1+µ2) = (µ3+µ4)' = c(1,1,-1,-1)/2
))

# All pairwise comparisons
pairs(emm, infer = c(T,T), adjust='none')

# Multiple comparisons
pairs(emm, infer = c(T,T), adjust='none')
pairs(emm, infer = c(T,T), adjust='tukey')
pairs(emm, infer = c(T,T), adjust='scheffe')
pairs(emm, infer = c(T,T), adjust='sidak')
pairs(emm, infer = c(T,T), adjust='bonferroni')

# Specific contrast: (µ1+µ2)/2 - (µ3+µ4)/2
contrast(emm, list(
  '(µ1+µ2)/2 - (µ3+µ4)/2' = c(1,1,-1,-1)/2 #R uses alphabetical order by default
))

#----- Simulate rejection rate for each of the above multiple comparisons -----

# Allocate space for results
n_sims <- 2000
sims <- matrix(nrow=n_sims, ncol=6)

# Comparisons to test
comps <- c('none', 'tukey', 'scheffe', 'sidak', 'bonferroni')

# For loop
for(i in 1:n_sims) {
  start_time <- Sys.time()
  # Simulate values
  n <- 20
  n_trt <- 4
  y <- rnorm(n*n_trt, mean = 30, sd = 5)
  trt <- rep(letters[1:n_trt], each = n)
  
  # Fit the model and get overall f-test p-value
  fit <- aov(y~trt)
  anova.rr <- as.numeric(anova(fit)$`Pr(>F)`[1]<0.05)
  
  # Fit estimated marginal means
  emm <- emmeans(fit, ~trt)
  
  # Fit multiple comparisons for each adjustment
  comp_rates <- numeric(length(comps))
  for(j in seq_along(comps)) {
    all_pairwise <- data.frame(pairs(emm, adjust=comps[j]))
    pairwise_rr <- any(all_pairwise$p.value < 0.05) # at least one rejection
    comp_rates[j] <- pairwise_rr
  }
  
  # Save results
  results <- c(anova.rr, comp_rates)
  sims[i,] <- results
  
  # Expectation of time spent on simulation
  end_time <- Sys.time()
  if(i==1) {
    initial_start_time <- start_time
    time_diff <- as.numeric(end_time-start_time, units='secs')
    amount_of_time <- time_diff*n_sims
    cat(glue::glue('This process will take approximately {round(amount_of_time, 1)} seconds to complete.'))
  }
  if(i == n_sims) {
    last_time_diff <- as.numeric(end_time - initial_start_time, units='secs')
    cat(glue::glue('\n\nActual process time was approximately {round(last_time_diff, 1)} seconds.'))
  }
}


# Display first few observations
head(sims)

# Calculate the mean rate of at least 1 Type 1 error
type1.error <- apply(sims, 2, mean)

# Display results
data.frame(
  Test = c('Overall F-test', comps),
  Type1.Error = type1.error
)

# Calculate cumulative means
cummeans <- apply(sims, 2, dplyr::cummean)

# Plot all six columns
matplot(
  cummeans,
  type = "l",
  lty = 1,
  lwd = 2,
  xlab = "Number of Simulations",
  ylab = "Cumulative Mean Rejection Rate",
  ylim = c(0, 0.15)
)

# Add reference line
abline(h = 0.05, lty = 2)

# Add legend
legend(
  "topright",
  legend = c("ANOVA", "None", "Tukey", "Scheffe", "Sidak", "Bonferroni"),
  col = 1:6,
  lty = 1,
  lwd = 2
)

