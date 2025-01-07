#calculate average per participant for geom_jitter points
average_values_abs <- data_all %>%
  dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type == "prom_spec" & url_code != "4746"
                & url_code != "2657") |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(average_color_angle_deviation = mean(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg = mean(color_angle_deviation, na.rm = TRUE))

# calculate median per participant
median_values_abs <- data_all %>%
  dplyr::filter(trial != "" & trial != "practice" & trial != "practice" & stimulus_type == "prom_spec" & url_code != "4746"
                & url_code != "2657") |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(median_color_angle_deviation = median(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg = mean(color_angle_abs_deviation, na.rm = TRUE))


#create avg value for barplot and for 95% interval
summary_stats_median <- median_values_abs %>%
  dplyr::filter(trial != "" & trial != "practice" & trial != "Practice" & stimulus_type == "prom_spec" & url_code != "4746"
                & url_code != "2657") |>
  group_by(load, trial) %>%
  summarise(overall_avg = median(median_color_angle_deviation, na.rm = TRUE),
            sd_value = sd(median_color_angle_deviation, na.rm = TRUE),
            ci_lower = overall_avg - qt(0.975, n()) * sd_value / sqrt(n()),
            ci_upper = overall_avg + qt(0.975, n()) * sd_value / sqrt(n()),
            overall_avg = mean(median_color_angle_deviation, na.rm = TRUE))

summary_stats_avg <- average_values_abs %>%
  dplyr::filter(trial != "" & trial != "practice" & trial != "Practice" & stimulus_type == "prom_spec" & url_code != "4746"
                & url_code != "2657") |>
  group_by(load, trial) %>%
  summarise(overall_avg = mean(average_color_angle_deviation, na.rm = TRUE),
            sd_value = sd(average_color_angle_deviation, na.rm = TRUE),
            ci_lower = overall_avg - qt(0.975, n()) * sd_value / sqrt(n()),
            ci_upper = overall_avg + qt(0.975, n()) * sd_value / sqrt(n()))


# change factor key to Baseline and Prospective memory
data_all$trial <- ifelse(data_all$trial == "baseline1", "baseline", 
                         data_all$trial)


## create main plot with jitter points and 95% interval (Poster Plot!)

color_values <- c("turquoise", "red", "green", "purple", "orange")  # Replace with actual trial colors
average_color <- "black"  # Color for the "Average" point


# Create main plot with jitter points and 95% interval
plot_median <- ggplot(data = median_values_abs, aes(x = trial, y = median_color_angle_deviation, fill = trial)) +
  geom_boxplot(alpha = 0.75) +
  geom_jitter(aes(x = trial, y = median_color_angle_deviation, color = trial),
              show.legend = FALSE, position = position_dodge(width = 0.8)) +
  geom_point(data = summary_stats_median, aes(x = trial, y = overall_avg, color = "Average"), size = 3) +
  scale_color_manual(values = c(color_values, "Average" = average_color), 
                     labels = c(levels(median_values_abs$trial), "Average")) +
  scale_fill_manual(values = color_values) +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
  labs(x = "Block type", y = "Average Color Deviation [°]", fill = "Block type", color = "") +
  scale_y_continuous(breaks = seq(0, 180, by = 10)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 10),  # Adjust x-axis title size
        axis.text.y = element_text(size = 10),   # Adjust y-axis label size
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), 
        legend.position = "top") +
  guides(color = guide_legend(nrow = 2)) +
  stat_compare_means(method = "t.test", label = "p.signif", 
                     aes(group = trial), 
                     position = position_nudge(x = 0.5))  # Add significance annotations


plot_median <-  plot_median + 
  geom_point(data = summary_stats_median, aes(x = trial, y = overall_avg, color = "Average"), size = 3) +
  scale_color_manual(values = c("black", "red", "blue"), labels = c("Mean",
                                                                    "Median per person",
                                                                    "Median per person")) +
  labs(color = "") +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
  theme(legend.title = element_text(size = 7), legend.text = element_text(size = 7)) +
  guides(color = guide_legend(nrow = 2))

print(plot_median)

# save the plot
ggplot2::ggsave(filename = "M09_results_PM.pdf", path = "/Users/tobiaskuehlwein/pm_color/teaching_files", plot = plot_median, width = 7, height = 5, dpi = 300)



m_sd_trial_load   <-  data_l1 |>  
  dplyr::filter(trial != "" & trial != "practice" & trial != "Practice" & stimulus_type == "prom_spec" & url_code != "4746"
                & url_code != "2657") |>
  group_by(trial, load) %>%
  summarise(color_avg = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_median = median(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_mad = mad(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(load)

m_sd_trial_load
