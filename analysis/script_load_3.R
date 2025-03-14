## R Script to analyse only load 3 for pm_color task

# clear workspace and console
cat('\014')

rm(list = ls())

# set working directory 
setwd("/Users/tobiaskuehlwein/pm_color")


# load in date for load one 
data_load3 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_3csv',header=TRUE)

# look at each VP to see if someone stopped in the middle
describeBy(data_load3$color_angle_abs_deviation, data_load3$participant_id)




# look at descriptive data based on trial and type and ignore the practice trial
data_load3 |>  
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  group_by(trial) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE))

# Do the same but split for each participant
data_load3 |>  
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  group_by(participant_id, trial) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE))



# create average per VP to use in boxplot for general outliers
load_3_vp_avg <- subset_data_l3_filtered %>%
  group_by(url_code, trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation))

#create average oer trial lot split for VP for barplot
trial_avg_l3 <- subset_data_l3_filtered %>%
  group_by(trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation))

outliers_l3 <- boxplot.stats(load_3_vp_avg$avg_color_angle_abs_deviation)$out


# boxplot for baseline and pm stplit into 6 groups with 20 stimuli each
load_3_base6_pm6_box <- ggplot(load_3_vp_avg, aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_boxplot(alpha = 0.8) + 
  geom_point(data = load_3_vp_avg, aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) +
  labs(x = "trial", y = "Color Angle Deviation abs") +  # Add axis labels
  theme_minimal()

# add url_code for outliers in boxplot
load_3_base6_pm6_box <- load_3_base6_pm6_box +
  geom_text(data = load_3_vp_avg[load_3_vp_avg$avg_color_angle_abs_deviation %in% outliers_l3, ],
            aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
            vjust = -1)

# barplot for baseline and pm stplit into 6 groups with 20 stimuli each
load_3_base6_pm6_bar <- ggplot(trial_avg_l3, aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" to plot values directly
  geom_point(data = load_3_vp_avg, aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) + 
  labs(x = "Trial", y = "Average Color Angle Deviation abs") +  # Add axis labels
  theme_minimal()
  
# add url_code for outliers in barplot
load_3_base6_pm6_bar <- load_3_base6_pm6_bar +
    geom_text(data = load_3_vp_avg[load_3_vp_avg$avg_color_angle_abs_deviation %in% outliers_l3, ],
              aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
              vjust = -1)

load_3_base6_pm6_box
load_3_base6_pm6_bar

# create a density plot for all deviations based on trial type, ignoring practice
data_load3 |>
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_histogram(aes(y = ..density.., bins = 100),
                 colour = 1, fill = "white") +
  geom_density(binwidth = 180) +
  facet_wrap(~ trial) +
  ggplot2::ggtitle("Density of color deviation by type load 1")


# violin plot with data points
data_load3 |>
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  ggplot(aes(x = color_angle_deviation, y = trial, fill = trial)) +
  geom_violin(alpha = 0.5) +
  geom_point(position = position_jitter(seed = 1, width = 0.01)) +
  theme(legend.position = "bottom")


# create density plot for each participant
data_load3 |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_histogram(aes(y = ..density.., bins = 100),
                 colour = 1, fill = "white") +
  geom_density(binwidth = 100) +
  facet_wrap(~ participant_id ~ trial) +
  ggplot2::ggtitle("Density of color deviation by type load 1 for all VP")


# create denisty with better visuals
data_load3 |>
  dplyr::filter(trial != "" & trial != "Practice" & url_code != 187575071) |>
  ggplot(aes(x = color_angle_deviation, y = ..scaled..)) +
  geom_density(adjust = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ trial + url_code) +
  ylab("Density (percentage)") +
  xlab("Color deviation in degrees") +
  theme(strip.text = element_text(size = 8, lineheight = 0.3),  # Adjust the size of the facet labels
        strip.background = element_rect(size = 0.1))


#create simple LME for hypothesis testing while ignoring the practice trial
data_load1_no_practice <- data_load3 %>%
  filter(trial != "" & trial != "Practice")

LME_l1 <- lmer(color_angle_abs_deviation ~ trial + stimulus_type + (1|trial_type)
               + (1|participant_id),
               data = data_load1_no_practice, na.action = na.exclude)

#create table to be used in paper two variants
sjPlot::tab_model(LME_l1, 
                  show.re.var= TRUE,
                  dv.labels= " Effects on color deviation in load 1")

