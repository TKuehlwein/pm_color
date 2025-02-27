#calculate average per participant for geom_jitter points
average_values_abs_pm <- data_all %>%
  dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type == "prom_spec" & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(average_color_angle_deviation_pm = mean(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg_pm = mean(color_angle_deviation, na.rm = TRUE))

# calculate median per participant
median_values_abs_pm <- data_all %>%
  dplyr::filter(trial != "" & trial != "practice" & trial != "practice" & stimulus_type == "prom_spec" & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(median_color_angle_deviation_pm = median(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg_pm = mean(color_angle_abs_deviation, na.rm = TRUE))


#create avg value for barplot and for 95% interval
summary_stats_median_pm <- median_values_abs_pm %>%
  dplyr::filter(trial != "" & trial != "practice" & trial != "Practice" & stimulus_type == "prom_spec" & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(load, trial) %>%
  summarise(overall_avg_pm = median(median_color_angle_deviation_pm, na.rm = TRUE),
            sd_value_pm = sd(median_color_angle_deviation_pm, na.rm = TRUE),
            ci_lower_pm = overall_avg_pm - qt(0.975, n()) * sd_value_pm / sqrt(n()),
            ci_upper_pm = overall_avg_pm + qt(0.975, n()) * sd_value_pm / sqrt(n()),
            overall_avg_pm = mean(median_color_angle_deviation_pm, na.rm = TRUE))

summary_stats_avg_pm <- average_values_abs_pm %>%
  dplyr::filter(trial != "" & trial != "practice" & trial != "Practice" & stimulus_type == "prom_spec" & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(load, trial) %>%
  summarise(overall_avg_pm = mean(average_color_angle_deviation_pm, na.rm = TRUE),
            sd_value_pm = sd(average_color_angle_deviation_pm, na.rm = TRUE),
            ci_lower_pm = overall_avg_pm - qt(0.975, n()) * sd_value_pm / sqrt(n()),
            ci_upper_pm = overall_avg_pm + qt(0.975, n()) * sd_value_pm / sqrt(n()))


# change factor key to Baseline and Prospective memory
data_all$trial <- ifelse(data_all$trial == "baseline1", "baseline", 
                         data_all$trial)


## create main plot with jitter points and 95% interval (Poster Plot!)

color_values <- c("turquoise", "red", "green", "purple", "orange")  # Replace with actual trial colors
average_color <- "black"  # Color for the "Average" point


# Create main plot with jitter points and 95% interval
plot_median_pm <- ggplot(data = median_values_abs_pm, aes(x = trial, y = median_color_angle_deviation_pm, fill = trial)) +
  geom_boxplot(alpha = 0.75) +
  geom_jitter(aes(x = trial, y = median_color_angle_deviation_pm, color = trial),
              show.legend = FALSE, position = position_dodge(width = 0.8)) +
  geom_point(data = summary_stats_median_pm, aes(x = trial, y = overall_avg_pm, color = "Average"), size = 3) +
  scale_color_manual(values = c(color_values, "Average" = average_color), 
                     labels = c(levels(median_values_abs_pm$trial), "Average")) +
  scale_fill_manual(values = color_values) +
  scale_x_discrete(labels = c("pm" = "Prospective \n memory")) +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
  labs(x = "Block type", y = "Average Color Deviation [°]", fill = "Block type", color = "") +
  scale_y_continuous(breaks = seq(0, 180, by = 10)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 15),
        axis.title.x = element_text(size = 15),  # Adjust x-axis title size
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 15), # Adjust x-axis label size
        legend.title = element_text(size = 23), 
        legend.text = element_text(size = 23), 
        legend.position = "top") +
  guides(color = guide_legend(nrow = 2))

plot_median_pm <-  plot_median_pm + 
  geom_point(data = summary_stats_median_pm, aes(x = trial, y = overall_avg_pm, color = "Average"), size = 3) +
  scale_color_manual(values = c("black", "red", "blue"), labels = c("Median of all Medians",
                                                                    "Median per person",
                                                                    "Median per person")) +
  labs(color = "") +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15)) +
  guides(color = guide_legend(nrow = 2))

plot_median_pm
# save the plot
ggplot2::ggsave(filename = "TeaP_2025_pm_only.pdf", path = "/Users/tobiaskuehlwein/pm_color/TeaP_2025", plot = plot_median_pm, width = 9, height = 7, dpi = 400)



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
