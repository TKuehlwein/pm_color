# Load required library
library(MASS)
library(tidyr)

# Number of participants
N <- 200

# Cohen's d values for the main effects
d_l1_b <- 0.22
d_l3_b <- 0.4
d_l5_b <- 0.65
d_l1_pm <- 0.25
d_l3_pm <- 0.67
d_l5_pm <- 0.8

# Cohen's d value for the interaction between l3_b and l3_pm
d_interaction <- 0.4  # Set your desired interaction effect size

# Base means for the conditions (can be any reasonable starting point)
base_mean_l1_b <- 0
base_mean_l3_b <- 0
base_mean_l5_b <- 0
base_mean_l1_pm <- 0
base_mean_l3_pm <- 0
base_mean_l5_pm <- 0

# Calculate means based on Cohen's d values and interaction effect
means <- c(
  base_mean_l1_b + d_l1_b,
  base_mean_l3_b + d_l3_b,
  base_mean_l5_b + d_l5_b,
  base_mean_l1_pm + d_l1_pm,
  base_mean_l3_pm + d_l3_pm,
  base_mean_l5_pm + d_l5_pm
)

# Incorporate the interaction effect into the means
interaction_effect <- function(condition_mean1, condition_mean2, d_interaction) {
  # Adjust means by adding the interaction effect
  return(condition_mean1 + condition_mean2 + d_interaction)
}

# Adjust means for the interaction between `l3_b` and `l3_pm`
#means[3] <- interaction_effect(base_mean_l3_b, base_mean_l5_b, d_interaction)
#means[6] <- interaction_effect(base_mean_l3_pm, base_mean_l5_pm, d_interaction)

# Correlation assumption between repeated measures
correlation <- 0.8

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
df_long <- df %>%
  pivot_longer(cols = -participant, names_to = "condition", values_to = "value")

# Display the first few rows of the reshaped data
#View(df_long)


df_separated <- df_long %>%
  separate(condition, into = c("load", "trial"), sep = 3)

#filter only load 3 for cohens d analysis
df_filtered <- df_long %>%
  filter(condition %in% c("l3_b", "l3_pm"))


# check if data is plausible
df_test <- df_long %>%
  group_by(condition) %>%
  summarise(
    color_avg = mean(value, na.rm = TRUE),
    color_median = median(value, na.rm = TRUE),
    color_dif_sd = sd(value, na.rm = TRUE),
    color_dif_min = min(value, na.rm = TRUE),
    color_dif_max = max(value, na.rm = TRUE)
  ) %>%
  arrange(condition)

df_test


#run t-test and cohens D
t.test(value ~ condition, paired = TRUE,  df_filtered)
cohens_d(df_filtered, value ~ condition, paired = TRUE)

run_two 
run_one 
#create plots for better overview interaction
ggplot(data = df_separated, aes(x = load, y = value, group = trial, color = trial)) +
  stat_summary(fun = mean, geom = "line", size = 1) +   # Line for the mean
  stat_summary(fun = mean, geom = "point", size = 3) +  # Points for the mean
  labs(
    title = "Average Value by Load and Trial",
    x = "Load",
    y = "Average Value",
    color = "Trial"
  ) +
  theme_minimal() # Use a minimal theme for a clean look
