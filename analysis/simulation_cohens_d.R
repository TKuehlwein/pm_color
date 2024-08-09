# Load required library
library(MASS)

# Number of participants
N <- 200

# Correlation assumption between repeated measures
correlation <- 0.8


# Cohen's d values for the 3x2 design
d_l1_b <- 0.2
d_l3_b <- 0.5
d_l5_b <- 0.8
d_l1_pm <- 0.3
d_l3_pm <- 0.95
d_l5_pm <- 1.0

dav1 = d_l1_b * sqrt(2 * (1 - correlation))
dav2 = d_l3_b * sqrt(2 * (1 - correlation))
dav3 = d_l5_b * sqrt(2 * (1 - correlation))
dav4 = d_l1_pm * sqrt(2 * (1 - correlation))
dav5 = d_l3_pm * sqrt(2 * (1 - correlation))
dav6 = d_l5_pm * sqrt(2 * (1 - correlation))

# Base mean for the first condition 
base_mean <- 0

# Calculate means based on Cohen's d values
# Assuming a standard deviation of 1 for simplicity
means <- c(
  base_mean + dav1,
  base_mean + dav2,
  base_mean + dav3,
  base_mean + dav4,
  base_mean + dav5,
  base_mean + dav6
)

# Define the correlation matrix for the 6 conditions
rho <- matrix(c(1, correlation, correlation, correlation, correlation, correlation,
                correlation, 1, correlation, correlation, correlation, correlation,
                correlation, correlation, 1, correlation, correlation, correlation,
                correlation, correlation, correlation, 1, correlation, correlation,
                correlation, correlation, correlation, correlation, 1, correlation,
                correlation, correlation, correlation, correlation, correlation, 1), 
              nrow = 6, ncol = 6)

# Simulate standardized data (mean = 0, SD = 1)
set.seed(123)  # For reproducibility
standardized_data <- mvrnorm(N, mu = rep(0, 6), Sigma = rho)

# Scale and shift the standardized data to match the desired means
scaled_data <- sweep(standardized_data, 2, colMeans(standardized_data), FUN = "-")
scaled_data <- sweep(scaled_data, 2, apply(scaled_data, 2, sd), FUN = "/")
final_data <- sweep(scaled_data, 2, means, FUN = "+")

# Convert to a data frame
df <- data.frame(
  participant = 1:N,
  l1_b = final_data[, 1],
  l3_b = final_data[, 2],
  l5_b = final_data[, 3],
  l1_pm = final_data[, 4],
  l3_pm = final_data[, 5],
  l5_pm = final_data[, 6]
)

# Reshape the data to long format
library(tidyr)
df_long <- df %>%
  # Pivot from wide to long format
  pivot_longer(cols = -participant, names_to = "condition", values_to = "value") %>%
  # Separate the 'condition' column into 'load' and 'trial'
  separate(condition, into = c("load", "trial"), sep = 2, remove = FALSE)

View(df_long)

df_test <- df_long %>%
  group_by(trial, load) %>%
  summarise(
    color_avg = mean(value, na.rm = TRUE),
    color_median = median(value, na.rm = TRUE),
    color_dif_sd = sd(value, na.rm = TRUE),
    color_dif_min = min(value, na.rm = TRUE),
    color_dif_max = max(value, na.rm = TRUE)
  ) %>%
  arrange(load)

df_test

#filter only load 3 for cohens d analysis
df_filtered <- df_long %>%
  filter(load %in% c("l3"))

#run t-test and cohens D
t.test(value ~ condition, paired = TRUE,  df_filtered)
cohens_d(df_filtered, value ~ condition, paired = TRUE)
