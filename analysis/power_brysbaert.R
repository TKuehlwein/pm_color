# Load required libraries
library(MASS)
library(dplyr)

# Define sample size
N = 200

# Define effect size d
dz1 = .4  # Effect size for the first variable
dz2 = .2  # Effect size for the second variable

# Define the correlation between the conditions
r = .9

# Calculate dav for the new design
dav1 = dz1 * sqrt(2 * (1 - r))
dav2 = dz2 * sqrt(2 * (1 - r))

# Define number of simulations
nSim = 5000

# Define alpha levels
alpha1 = .05  # Alpha level for the omnibus ANOVA
alpha2 = .10 / 3  # Alpha level for post hoc tests (Bonferroni correction)


# Create vectors to store p-values
p1 <- numeric(nSim)  # p-value for the omnibus ANOVA
p2 <- numeric(nSim)  # p-value for post hoc test 1 (e.g., between condition 1 and 2)
p3 <- numeric(nSim)  # p-value for post hoc test 2 (e.g., between condition 1 and 3)
p4 <- numeric(nSim)  # p-value for post hoc test 3 (e.g., between condition 2 and 3)

# Define correlation matrix for 3x2 design
rho <- matrix(c(
  1, r, r, r, r, r,
  r, 1, r, r, r, r,
  r, r, 1, r, r, r,
  r, r, r, 1, r, r,
  r, r, r, r, 1, r,
  r, r, r, r, r, 1
), nrow = 6, ncol = 6)

# Define participant codes
part <- paste("part", seq(1:N))

for (i in 1:nSim) {  # For each simulated experiment

  # Simulate data
  data = mvrnorm(n = N, mu = c(0, 0, 0, 0, 0, 0), Sigma = rho)
  data[, 2] = data[, 2] + dav1
  data[, 3] = data[, 3] + dav2
  data[, 4] = data[, 4] + dav1
  data[, 5] = data[, 5] + dav2
  data[, 6] = data[, 6] + dav1 + dav2
  
  datalong = c(data[, 1], data[, 2], data[, 3], data[, 4], data[, 5], data[, 6])
  
  # Define conditions for the 3x2 design
  conds = factor(rep(c("l1_b", "l1_pm", "l3_b", "l3_pm", "l5_b", "l5_pm"), each = N))
  
  # Define participant IDs
  partID = factor(rep(part, times = 6))
  
  # Create data frame
  output <- data.frame(partID, conds, datalong)
  
  # Perform ANOVA
  test <- aov(datalong ~ conds + Error(partID / conds), data = output)
  tests <- summary(test)
  
  # Store p-values
  p1[i] <- tests$'Error: partID:conds'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[, 1], data[, 2], paired = TRUE)$p.value
  p3[i] <- t.test(data[, 1], data[, 3], paired = TRUE)$p.value
  p4[i] <- t.test(data[, 2], data[, 3], paired = TRUE)$p.value
}

# Calculate results
supportH1 <- sum((p1 < alpha1) & (p2 < alpha2) & (p3 < alpha2) & (p4 < alpha2)) / nSim
supportOMNI <- sum(p1 < alpha1) / nSim

# Print results
cat("Power of the test (support H1): ", supportH1, "\n")
cat("Omnibus ANOVA significance (support OMNI): ", supportOMNI, "\n")
