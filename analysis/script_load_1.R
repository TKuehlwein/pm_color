## R Script to analyse only load 1 for pm_color task

# clear workspace and console
cat('\014')

rm(list = ls())

# set working directory 
setwd("/Users/tobiaskuehlwein/pm_color")


# load in date for load one 
data_load1_v2 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_1_v2.csv',header=TRUE)

# look at each VP to see if someone stopped in the middle
describeBy(data_load1_v2$color_angle_abs_deviation, data_load1_v2$participant_id)


# look at discriptive data based on trial and type and ignore the practice trial
data_load1_v2 |>  
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  group_by(trial) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE))

# Do the same but split for each participant
data_load1_v2 |>  
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  group_by(participant_id, trial) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE))

# create a density plot for all deviations based on trial type, ignoring practice
data_load1_v2 |>
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_histogram(aes(y = ..density.., bins = 100),
                 colour = 1, fill = "white") +
  geom_density() +
  facet_wrap(~ trial) +
  ggplot2::ggtitle("Density of color deviation by type load 1")


# create density plot for each participant

data_load1_v2 |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_histogram(aes(y = ..density.., bins = 100),
                 colour = 1, fill = "white") +
  geom_density(binwidth = 100) +
  facet_wrap(~ participant_id ~ trial) +
  ggplot2::ggtitle("Density of color deviation by type load 1 for all VP")


#create simple LME for hypothesis testing while ignoring the practice trial

data_load1_no_practice <- data_load1_v2 %>%
  filter(trial != "" & trial != "Practice")

LME_l1 <- lmer(color_angle_abs_deviation ~ trial + stimulus_type + (1|trial_type)
               + (1|participant_id),
               data = data_load1_no_practice, na.action = na.exclude)

#create table to be used in paper two variants
sjPlot::tab_model(LME_l1, 
                  show.re.var= TRUE,
                  dv.labels= " Effects on color deviation in load 1")

