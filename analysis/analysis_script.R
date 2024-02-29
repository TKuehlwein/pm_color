# Script to analyse data from color pm study
# Created by Tobias Kühlwein


# clear workspace
rm(list=ls())

# read in all data files and clear them of unused rows and data points 

data_all = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_all.csv',header=TRUE)
data_l1 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_1_v2.csv',header=TRUE)
data_l3 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_3.csv',header=TRUE)
data_l5 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_5.csv',header=TRUE)
data_l3_base2 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_3_baseline_1_2.csv',header=TRUE)

filtered_data_l1 <- subset_data_l1 %>% filter(trial != "")
filtered_data_l3 <- subset_data_l2 %>% filter(trial != "")
filtered_data_l5 <- subset_data_l5 %>% filter(trial != "")

subset_data_l1 <- subset(data_l1, select = c("observation", "trial", "load",
                                           "stimulus_type", "ended_on", "url_code",
                                           "trial_type", "trial_seq", "color_offset", 
                                           "color_angle_test", "color_angle_deviation",
                                           "color_angle_abs_deviation" ))


subset_data_l2 <- subset(data_l3, select = c("observation", "trial", "load",
                                        "stimulus_type", "ended_on", "url_code",
                                        "trial_type", "trial_seq", "color_offset", 
                                        "color_angle_test", "color_angle_deviation",
                                        "color_angle_abs_deviation" ))

subset_data_l5 <- subset(data_l5, select = c("observation", "trial", "load",
                                        "stimulus_type", "ended_on", "url_code",
                                        "trial_type", "trial_seq", "color_offset", 
                                        "color_angle_test", "color_angle_deviation",
                                        "color_angle_abs_deviation" ))

#write the subsets into their own csv files
write.csv(filtered_data_l1, file = "load_1_filter.csv", row.names = TRUE)
write.csv(filtered_data_l3, file = "load_3_filter.csv", row.names = TRUE)
write.csv(filtered_data_l5, file = "load_5_filter.csv", row.names = TRUE)



# load all packages might needed
library(psych)
library(tibble)
library(rstatix)
library(ggplot2)
library(dplyr)
library(car)
library(multcomp)
library(emmeans)
library(stargazer)
library(lme4)


describeBy(data_all$color_angle_abs_deviation, data_all$url_code)


# look for outliers by averaging the median and creating a plot for it
overall_median <-  data_all |>  
                    dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec"
                                  & url_code != "187575071") |>
                    group_by(url_code, load) %>%
                    summarise(overall_median_vp  = median(color_angle_abs_deviation, na.rm = TRUE),)%>%
                    arrange(url_code)

overall_median_no_load <-  data_all |> 
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec"
                & url_code != "187575071") |>
  group_by(url_code) %>%
  summarise(overall_median_vp  = median(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(url_code)

# create one plot for all, not splitting for load or trial type
median_plot_nol_not <-  ggplot(overall_median_no_load, aes(x = "", y = overall_median_vp)) +
                          geom_boxplot(fill = "lightblue", color = "blue", outlier.colour = "red") + 
                          geom_jitter(aes(y = overall_median_vp), color = "black", alpha = 0.5) + 
                          labs(x = NULL, y = "Overall Median Color Angle Deviation") +
                          theme_minimal()

# create the same plot for all 3 loads seperat not looking at trial type
median_plot_not <- ggplot(overall_median, aes(x = "load", y = overall_median_vp)) +
                    geom_boxplot(fill = "lightblue", color = "blue", outlier.colour = "red") + 
                    geom_jitter(aes(y = overall_median_vp), color = "black", alpha = 0.5) + 
                    labs(x = NULL, y = "Overall Median Color Angle Deviation") +
                    facet_wrap(~ load)
                    theme_minimal()


# m and sd for each trial type per load
    data_l5_base2 |>  
      dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec"
                    & url_code != "187575071") |>
      group_by(trial, load) %>%
      summarise(color_avg = mean(color_angle_abs_deviation, na.rm = TRUE),
                color_median = median(color_angle_abs_deviation, na.rm = TRUE),
                color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
                color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
                color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
      arrange(load)
  

# m, sd, min, max for each trial per load per vp
data_l5 |>  
  dplyr::filter(trial != "" & trial != "Practice") |>
  group_by(trial, url_code) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(trial)



#rename col if needed
colnames(data)
names(df2)[names(df2) == "Bedingung"] <- "Condition"

# show potential outliers
sum(data_all$color_angle_abs_deviation, na.rm = TRUE)
count_within_range <- sum(data_all$color_angle_abs_deviation >= 100, na.rm = TRUE)
print(count_within_range)

# filter out potential outliers
filtered_data <- subset(data_all_v4, !(color_angle_abs_deviation >= 100))


#first LME 
LME_l1 <- lmer(color_angle_abs_deviation ~ trial + load + stimulus_type + (1|trial_type),
              data = data, na.action = na.exclude)



#create table to be used in paper two variants
sjPlot::tab_model(LME_l3, 
                  show.re.var= TRUE,
                  dv.labels= "load 3")

stargazer(LME_2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# lots of testing around stuff, not all useful

# density plit for all
data_all |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot2::ggplot(ggplot2::aes(y = color_angle_deviation, x = trial, group = trial, color = trial)) +
  ggplot2::facet_wrap( ~ trial) +
  ggplot2::geom_density() + 
  ggplot2::ggtitle("Density of color deviation by type load 5")

data_l1_base2 |>
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec"
                & url_code != "187575071") |>
  ggplot(aes(x = color_angle_deviation, fill = trial)) + 
  geom_histogram( bins = 180) +
  facet_wrap(~ trial)



data_l1_base2 |>
  dplyr::filter(trial != "" & trial != "Practice" & url_code != 402695612) |>
  ggplot(aes(x = color_angle_deviation, colour = trial)) + 
    geom_histogram(bins = 60) + 
  facet_wrap(~ load)

data_l1_base2 |>
  dplyr::filter(trial != "" & trial != "Practice" & url_code != 402695612) |>
  ggplot(aes(x = color_angle_deviation)) +
    geom_density() +
  ggplot2::facet_wrap(~ load )
  

data_l1_base2 |>
  dplyr::filter(trial != "" & trial != "Practice" & url_code != 402695612) |>
  ggplot(aes(x = color_angle_deviation)) + 
    geom_histogram(aes(y = ..density.., bins = 100),
                   colour = 1, fill = "white") +
    geom_density() +
    facet_wrap(~ load, nrow = 3)
    

## Some useful plots


average_values <- data_all %>%
  filter(trial != "" & trial != "Practice") %>%
  group_by(url_code, trial, load) %>%
  summarise(average_color_angle_deviation = mean(color_angle_deviation, na.rm = TRUE))


data_all |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation, y = trial)) +
    geom_violin(aes(x=color_angle_deviation, y = trial, fill = load), alpha = 0.5) +
    geom_point(data = average_values, aes(x = average_color_angle_deviation, y = trial), 
               position = position_dodge(width = 0.2), color = "black", size = 3) +
    stat_ellipse() +
    facet_wrap(~ load) + 
    theme(legend.position = "bottom",
          strip.text = element_blank())
  
data_all |>
  dplyr::filter(trial != "" & trial != "Practice") |>  
  ggplot(aes(x = color_angle_deviation, y = trial)) +
  geom_boxplot( aes(x = color_angle_deviation, y = trial)) +
  geom_point(data = average_values, aes(x = average_color_angle_deviation, y = trial), 
             position = position_dodge(width = 0.2), color = "black", size = 3) +
  stat_ellipse(color = 2,
               linetype = 2,
               lwd = 1.2) +
  facet_wrap(~ load)


# density plot per load per trial
data_l1_base2 |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_freqpoly(bins = 180) +
  facet_wrap(~ trial, nrow = 3)

#different density plot
data_l1_base2 |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation, y = ..scaled..)) +
    geom_density(adjust = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~ trial, nrow = 3) +
    ylab("Density (percentage)") + 
    xlab("Color deviation in degrees")

#Density plot per load per trial per vp
data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & url_code != 187575071) |>
  ggplot(aes(x = color_angle_deviation, y = ..scaled..)) +
  geom_density(adjust = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ load + trial + url_code) +
  ylab("Density (percentage)") +
  xlab("Color deviation in degrees") +
  theme(strip.text = element_text(size = 8, lineheight = 0.3),  # Adjust the size of the facet labels
        strip.background = element_rect(size = 0.1))



#more plots to have some choice
(prelim_plot <- ggplot(data, aes(x = color_angle_abs_deviation, y = trial, color = trial)) +
    geom_point() +
    geom_smooth(color = "Red" ,method = "lm"))

(split_plot <- ggplot(aes(color_angle_deviation, trial), color = red, data = data) + 
    geom_point() + 
    facet_wrap(~ trial_seq) + 
    xlab("Trial1") + 
    ylab("Trial2"))




#create two boxlpots in one fig
par(mfrow=c(1,2))
boxplot(color_angle_deviation ~ trial, na.rm = TRUE, data = data_l1_base2)
boxplot(average_color_angle_deviation ~ trial, data = average_values)


summary_stats <- average_values_abs %>%
  group_by(load, trial) %>%
  summarise(
    mean_value = mean(average_color_angle_deviation, na.rm = TRUE),
    sd_value = sd(average_color_angle_deviation, na.rm = TRUE)
  )


average_values_abs <- data_all %>%
  filter(trial != "" & trial != "Practice") %>%
  group_by(url_code, trial, load) %>%
  summarise(average_color_angle_deviation = mean(color_angle_abs_deviation, na.rm = TRUE))


ggplot(average_values_abs, aes(x = trial, y = average_color_angle_deviation, fill = trial)) +
  geom_boxplot(position = "dodge", alpha = 0.5) +
  geom_jitter(aes(color = trial), show.legend = FALSE, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("black", "blue")) +
  facet_wrap(~ load,  labeller = labeller(load = function(variable) paste("Load", variable))) +
  labs(x = "Trial type", y = "Average Color Angle Deviation", fill = "Trial type")


# create overall barplot for all loads and data points average

#calculate average per participant for geom_jitter points
average_values_abs <- data_all %>%
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec") |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(average_color_angle_deviation = mean(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg = mean(color_angle_deviation, na.rm = TRUE)

# calculate median per participant
median_values_abs <- data_all %>%
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec") |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(median_color_angle_deviation = median(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg = mean(color_angle_abs_deviation, na.rm = TRUE))


#create avg value for barplot and for 95% interval
summary_stats_median <- median_values_abs %>%
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec") |>
  group_by(load, trial) %>%
  summarise(overall_avg = median(median_color_angle_deviation, na.rm = TRUE),
            sd_value = sd(median_color_angle_deviation, na.rm = TRUE),
            ci_lower = overall_avg - qt(0.975, n()) * sd_value / sqrt(n()),
            ci_upper = overall_avg + qt(0.975, n()) * sd_value / sqrt(n()),
            overall_avg = mean(median_color_angle_deviation, na.rm = TRUE))

summary_stats_avg <- average_values_abs %>%
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec") |>
  group_by(load, trial) %>%
  summarise(overall_avg = mean(average_color_angle_deviation, na.rm = TRUE),
            sd_value = sd(average_color_angle_deviation, na.rm = TRUE),
            ci_lower = overall_avg - qt(0.975, n()) * sd_value / sqrt(n()),
            ci_upper = overall_avg + qt(0.975, n()) * sd_value / sqrt(n()))


## create main plot with jitter points and 95% interval (Poster Plot!)
plot_median <-   ggplot(data = median_values_abs, aes(x = trial, y = median_color_angle_deviation, fill = trial)) +
                  geom_boxplot(alpha = 0.8) +
                  geom_jitter(aes(x = trial, y = median_color_angle_deviation, color = trial),
                              show.legend = FALSE, position = position_dodge(width = 0.8)) +
                  scale_color_manual(values = c("black", "blue")) +
                  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
                  labs(x = "Trial type", y = "Median Color Angle Deviation", fill = "Trial type") +
                  theme_minimal() +
  theme(strip.text = element_text(size = 12), axis.text.x = element_text(size = 10))
  
plot_median <-  plot_median + 
                  geom_point(data = summary_stats_median, aes(x = trial, y = overall_avg, color = "Average"), size = 3) +
                  scale_color_manual(values = c("black", "red", "blue"), labels = c("Overall median",
                                                                                    "median per person",
                                                                                    "median per person")) +
                  labs(color = "Mean and\ndistinct data\npoints") +
                  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
                  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 3))


# plot for mean instead of median
plot_average <-   ggplot(summary_stats_avg, aes(x = trial, y = overall_avg, fill = trial)) +
                    geom_bar(stat = "identity", position = "identity") +
                    geom_jitter(data = average_values_abs, aes(x = trial, y = average_color_angle_deviation, color = trial),
                                show.legend = FALSE, position = position_dodge(width = 0.8)) +
                    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
                    scale_color_manual(values = c("black", "blue")) +
                    facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
                    labs(x = "Trial type", y = "Average Color Angle Deviation", fill = "Trial type") +
                    theme_minimal()






# Define a vector of distinct colors
distinct_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                     "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                     "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", 
                     "#c49c94", "#f7b6d2", "#c7c7c7")

# Create the bar plot with distinct colors
bar_plot <- ggplot(x, aes(x = trial, y = color_median, fill = url_code)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ url_code + load, labeller = labeller(load = function(variable) paste("Load", variable)), ncol = 6) +
  labs(x = "X Axis Label", y = "Y Axis Label", title = "Bar Plot") +
  theme_minimal() +
  scale_fill_manual(values = distinct_colors)

# show plots
plot_median
median_plot_nol_not
median_plot_not
plot_average
bar_plot

par(mfrow=c(1,1))

