#check for outliers and if subjects produced data for all sessions


library(dplyr)

#check number of unique url_codes per load
number_N <- data_all |>
  dplyr::filter(load == "3") |>
  dplyr::pull(url_code) |>  
  unique() |>  
  length()  


# Count number of data points per participant and load condition
participant_check <- data_all %>%
  group_by(participant_id, load) %>%  # Group by participant and load
  summarise(n = n(), .groups = "drop") %>%  # Count number of rows (data points)
  filter(load %in% c(1, 3, 5)) %>%  # Only consider loads 1, 3, and 5
  spread(key = load, value = n, fill = 0) %>%  # Spread to wide format for easy checking
  mutate(all_sessions = (`1` >= 120) & (`3` >= 120) & (`5` >= 120))  # Check if all conditions have >= 120 points

# Get participants who have not met the requirement
participants_missing_data <- participant_check %>%
  filter(!all_sessions)

# Print summary
if (nrow(participants_missing_data) == 0) {
  print("All participants have at least 120 data points for load 1, 3, and 5.")
} else {
  print("Some participants did not meet the required data points:")
  print(participants_missing_data)
}

print(participants_missing_data, n=34)

    

test   <-  data_all |>  
  dplyr::filter(url_code == "2634" & trial != "practice") |>
  group_by(trial, load) %>%
  summarise(color_avg = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_median = median(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_mad = mad(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(load)

test

print(test, n=20)

