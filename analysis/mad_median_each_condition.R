## calculate mad and median for each trial and load on their own for further use
# (there has to be a better way of doing this, but I don't know it)

#load one baseline block
mad_color_l1_base <- data_all %>%
  filter(trial != "practice" & trial != "pm" & load != "3" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color_l1_base <- data_all %>%
  filter(trial != "practice" & trial != "pm" & load != "3" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))

#load one pm block
mad_color_l1_pm <- data_all %>%
  filter(trial != "practice" & trial != "baseline" & load != "3" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color_l1_pm <- data_all %>%
  filter(trial != "practice" & trial != "baseline" & load != "3" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))

#load three baseline block
mad_color_l3_base <- data_all %>%
  filter(trial != "practice" & trial != "pm" & load != "1" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color_l3_base <- data_all %>%
  filter(trial != "practice" & trial != "pm" & load != "1" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))


# # change values for lowest effect of interest
# median_color_l3_pm <- median_color_l3_pm %>%
#   mutate(median_color_angle_abs_deviation = 21.1518)
# 
# # change values for lowest effect of interest
# mad_color_l3_pm <- mad_color_l3_pm %>%
#   mutate(mad_color_angle_abs_deviation = 7.72523)



#load three pm block
mad_color_l3_pm <- data_all %>%
  filter(trial != "practice" & trial != "baseline" & load != "1" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color_l3_pm <- data_all %>%
  filter(trial != "practice" & trial != "baseline" & load != "1" & load != "5") %>%  # Exclude 'everything irrelevant' trial
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))

#load five baseline block
mad_color_l5_base <- data_all %>%
  filter(trial != "practice" & trial != "pm" & load != "1" & load != "3") %>%  # Exclude 'everything irrelevant' trial
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color_l5_base <- data_all %>%
  filter(trial != "practice" & trial != "pm" & load != "1" & load != "3") %>%  # Exclude 'everything irrelevant' trial
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))

#load five pm block
mad_color_l5_pm <- data_all %>%
  filter(trial != "practice" & trial != "baseline" & load != "1" & load != "3") %>%  # Exclude 'everything irrelevant' trial
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color_l5_pm <- data_all %>%
  filter(trial != "practice" & trial != "baseline" & load != "1" & load != "3") %>%  # Exclude 'everything irrelevant' trial
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))




# cohen's d effect size
d <- 0.8

# calculate the mean difference for the two conditions
#for each condition again
#l1 base
mean_color_diff_l1_base <- d * mad_color_l1_base
mad_color_diff_l1_base <- d * mad_color_l1_base

#for each condition again
#l1 pm
mean_color_diff_l1_pm <- d * median_color_l1_pm
mad_color_diff_l1_pm <- d * mad_color_l1_pm

#for each condition again
#l3 base
mean_color_diff_l3_base <- d * median_color_l3_base
mad_color_diff_l3_bse <- d * mad_color_l3_base

#for each condition again
#l3 pm
mean_color_diff_l3_pm <- d * median_color_l3_pm
mad_color_diff_l3_pm <- d * mad_color_l3_pm

#for each condition again
#l5 base
mean_color_diff_l5_base <- d * median_color_l5_base
mad_color_diff_l5_base <- d * mad_color_l5_base

#for each condition again
#l5 pm
mean_color_diff_l5_pm <- d * median_color_l5_pm
mad_color_diff_l5_pm <- d * mad_color_l5_pm

# adjust mean for the experimental group

#l1 base
mean_color_exp_l1_base <- median_color_l1_base - mean_color_diff_l1_base
mad_color_exp_l1_base <- mad_color_l1_base - mad_color_diff_l1_base

#l1 pm
mean_color_exp_l1_pm <- median_color_l1_pm - mean_color_diff_l1_pm
mad_color_exp_l1_pm <- mad_color_l1_pm - mean_color_diff_l1_pm

#l3 base
mean_color_exp_l3_base <- median_color_l3_base - mean_color_diff_l3_base
mad_color_exp_l3_base <- mad_color_l3_base - mean_color_diff_l3_base

#l3 pm
mean_color_exp_l3_pm <- median_color_l3_pm - mean_color_diff_l3_pm
mad_color_exp_l3_pm <- mad_color_l3_pm - mean_color_diff_l3_pm

#l5 base
mean_color_exp_l5_base <- median_color_l5_base - mean_color_diff_l5_base
mad_color_exp_l5_base <- mad_color_l5_base - mean_color_diff_l5_base

#l5 pm
mean_color_exp_l5_pm <- median_color_l5_pm - mean_color_diff_l5_pm
mad_color_exp_l5_pm <- mad_color_l5_pm - mean_color_diff_l5_pm
