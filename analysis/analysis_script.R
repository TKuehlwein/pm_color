# Script to analyse data from color pm study
# Created by Tobias Kühlwein


# clear workspace
rm(list=ls())

# read in all data files and clear them of unused rows and data points 

subset_data_l1_filtered = read.csv ("/Users/tobiaskuehlwein/pm_color/analysis/subset_data_l1_filtered.csv", header = TRUE)
subset_data_l3_filtered = read.csv ("/Users/tobiaskuehlwein/pm_color/analysis/load_3_filter_06.csv", header = TRUE)
subset_data_l5_filtered = read.csv ("/Users/tobiaskuehlwein/pm_color/analysis/subset_data_l5_filtered.csv", header = TRUE)
subset_data_all_filtered = read.csv("/Users/tobiaskuehlwein/pm_color/analysis/data_all_b6_pm6.csv", header = TRUE)
B1l1_P6l3 = read.csv("/Users/tobiaskuehlwein/pm_color/analysis/trial_split_by_load_trial_6.csv", header = TRUE)
data_all = read.csv('/Users/tobiaskuehlwein/pm_color/full_analysis/data_all.csv',header=TRUE)
data_l1 = read.csv('/Users/tobiaskuehlwein/pm_color/teaching_files/load_1.csv',header=TRUE)
data_l3 = read.csv('/Users/tobiaskuehlwein/pm_color/teaching_files/load_3.csv',header=TRUE)
data_l5 = read.csv('/Users/tobiaskuehlwein/pm_color/teaching_files/load_5.csv',header=TRUE)
data_l3_base2 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_3_baseline_1_2.csv',header=TRUE)
histo_all = read.csv("/Users/tobiaskuehlwein/pm_color/analysis/all_load_test.csv", header = TRUE)

prolific_s1 = read.csv("/Users/tobiaskuehlwein/pm_color/full_analysis/prolific_s1.csv", header = TRUE)
prolific_s2 = read.csv("/Users/tobiaskuehlwein/pm_color/full_analysis/prolific_s2.csv", header = TRUE)
prolific_s3 = read.csv("/Users/tobiaskuehlwein/pm_color/full_analysis/prolific_s3.csv", header = TRUE)
prolific_all = read.csv("/Users/tobiaskuehlwein/pm_color/full_analysis/prolific_all.csv", header = TRUE)

prolific_s1 <- prolific_s1 %>% filter(color_angle_deviation != "")
prolific_s2 <- prolific_s2 %>% filter(color_angle_deviation != "")
prolific_s3 <- prolific_s3 %>% filter(color_angle_deviation != "")
data_all <- data_all %>% filter(color_angle_deviation != "")

## add "url_code" for sona data, participant_id for prolific
prolific_s1 <- subset(prolific_s1, select = c("participant_id", "observation", "trial", "load",
                                           "stimulus_type", "ended_on",
                                           "trial_type", "trial_seq", "color_offset", 
                                           "color_angle_test", "color_angle_deviation",
                                           "color_angle_abs_deviation", "stimulus_seq"))


prolific_s2  <- subset(prolific_s2, select = c("participant_id","observation", "trial", "load",
                                        "stimulus_type", "ended_on",
                                        "trial_type", "trial_seq", "color_offset", 
                                        "color_angle_test", "color_angle_deviation",
                                        "color_angle_abs_deviation", "stimulus_seq" )) 

prolific_s2 <- prolific_s2 %>% mutate(load = 3)

prolific_s3 <- subset(prolific_s3, select = c("participant_id","observation", "trial", "load",
                                        "stimulus_type", "ended_on",
                                        "trial_type", "trial_seq", "color_offset", 
                                        "color_angle_test", "color_angle_deviation",
                                        "color_angle_abs_deviation", "stimulus_seq" )) 
prolific_s3 <- prolific_s3 %>% mutate(load = 5)


#write the subsets into their own csv files
write.csv(prolific_s1, file = "prolific_s1.csv", row.names = TRUE)
write.csv(prolific_s2, file = "prolific_s2.csv", row.names = TRUE)
write.csv(prolific_s3, file = "prolific_s3.csv", row.names = TRUE)
write.csv(data_all, file = "load_all_filter_06.csv", row.names = TRUE)


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
library(ggpubr)


describeBy(data_l1$color_angle_abs_deviation, data_l1$trial)
describeBy(data_l3$color_angle_abs_deviation, data_l3$trial)
describeBy(data_l5$color_angle_abs_deviation, data_l5$trial)



# look for outliers by averaging the median and creating a plot for it
overall_median <-  data_all |>  
                    dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type != "prom_spec") |>
                    group_by(load, trial) %>%
                    summarise(overall_median_vp  = median(color_angle_abs_deviation, na.rm = TRUE),)%>%
                    arrange(load)

subset_data_l3_filtered |> 
  dplyr::filter(trial != "" & trial != "Practice"  & url_code != "187575071" & 
                  stimulus_type != "prom_spec") |>
  group_by(trial_type) %>%
  summarise(overall_median_vp  = median(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(trial_type)

# create one plot for all, not splitting for load or trial type
median_plot_nol_not <-  ggplot(overall_median_no_load, aes(x = "", y = overall_median_vp)) +
                          geom_boxplot(fill = "lightblue", color = "blue", outlier.colour = "red") + 
                          geom_jitter(aes(y = overall_median_vp), color = "black", alpha = 0.5) + 
                          labs(x = NULL, y = "Overall Median Color Angle Deviation") +
                          theme_minimal()

# create the same plot split for all 3 loads, not looking at trial type
median_plot_not <- ggplot(overall_median, aes(x = "load", y = overall_median_vp)) +
                    geom_boxplot(fill = "lightblue", color = "blue", outlier.colour = "red") + 
                    geom_jitter(aes(y = overall_median_vp), color = "black", alpha = 0.5) + 
                    labs(x = NULL, y = "Overall Median Color Angle Deviation") +
                    facet_wrap(~ load)
                    theme_minimal()


# m and sd for each trial type per load
m_sd_trial_load   <-  data_all |>  
      dplyr::filter(trial != "" & trial != "practice" & trial != "Practice" & stimulus_type != "prom_spec"
                    & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
      group_by(trial, load) %>%
      summarise(color_avg = mean(color_angle_abs_deviation, na.rm = TRUE),
                color_median = median(color_angle_abs_deviation, na.rm = TRUE),
                color_dif_mad = mad(color_angle_abs_deviation, na.rm = TRUE),
                color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
                color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
      #arrange(desc(color_median))
      arrange(load)

m_sd_trial_load
print(m_sd_trial_load, n=20)
# m, sd, min, max for each trial per load per vp
outliers_vp <- data_all |>  
  dplyr::filter(trial != "" & trial != "Practice") |>
  group_by(trial, url_code, load) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(load)



#rename col if needed
colnames(data)
names(df2)[names(df2) == "Bedingung"] <- "Condition"

# show potential outliers
sum(data_all$color_angle_abs_deviation, na.rm = TRUE)
count_within_range <- sum(data_all$color_angle_abs_deviation >= 100, na.rm = TRUE)
print(count_within_range)

# filter out potential outliers
data_all <- subset(data_all, !(trial == Practice))
data_all <- subset(data_all, trial != "practice")



#first LME 
LME_l1 <- lmer(color_angle_abs_deviation ~ load + trial + stimulus_type + trial*load + (1|trial_type),
              data = data_all, na.action = na.exclude)


summary(LME_l1)
anova(LME_l1)
summ(LME_1)

#create table to be used in paper two variants
sjPlot::tab_model(LME_l1, 
                  show.re.var= TRUE,
                  dv.labels= "Linear mixed model")

stargazer(LME_l1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# lots of testing around stuff, not all useful

# density plit for all
data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" ) |>
  ggplot2::ggplot(ggplot2::aes(y = color_angle_deviation, x = trial, group = trial, color = trial)) +
  ggplot2::facet_wrap( ~ trial) +
  ggplot2::geom_density() + 
  ggplot2::ggtitle("Density of color deviation by type load 5")

data_all |>
  dplyr::filter(trial != "" & trial != "Practice"  & stimulus_type != "prom_spec") |>
  ggplot(aes(y = color_angle_deviation, fill = stimulus_seq, group = stimulus_seq)) + 
  geom_histogram( bins = 180) +
  facet_wrap(~ trial)


avg_data <- data_all %>%
     filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" & url_code != "187575071") %>%
     group_by(stimulus_seq, trial) %>%
     summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation, na.rm = TRUE))

histo_stim_seq <- avg_data |>
        dplyr::filter(trial != "" & trial != "Practice") |>
          ggplot(aes(x = stimulus_seq, y = avg_color_angle_abs_deviation)) +
          geom_bar(stat = "identity", fill = "blue") +
          geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.5) + # Add error bars for variability
          labs(x = "Stimulus Sequence", y = "Average Color Angle Deviation") +
          theme_minimal() +
          facet_wrap(~ trial)


data_l1 |>
  dplyr::filter(trial != "" & trial != "Practice") |>
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
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec") |>
  ggplot(aes(x = color_angle_deviation, y = trial)) +
    geom_violin(aes(x=color_angle_deviation, y = trial, fill = load), alpha = 0.5) +
    geom_point(data = average_values, aes(x = average_color_angle_deviation, y = trial), 
               position = position_dodge(width = 0.2), color = "black", size = 3) +
    stat_ellipse() +
    facet_wrap(~ load) + 
    theme(legend.position = "bottom",
          strip.text = element_blank())
  
data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec") |>  
  ggplot(aes(x = color_angle_deviation, y = trial)) +
  geom_boxplot( aes(x = color_angle_deviation, y = trial)) +
  geom_point(data = average_values, aes(x = average_color_angle_deviation, y = trial), 
             position = position_dodge(width = 0.2), color = "black", size = 3) +
  stat_ellipse(color = 2,
               linetype = 2,
               lwd = 1.2) +
  facet_wrap(~ load)


# density plot per load per trial
data_l1 |>
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec") |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_freqpoly(bins = 180) +
  facet_wrap(~ trial, nrow = 3)

#different density plot
data_l1 |>
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec") |>
  ggplot(aes(x = color_angle_deviation, y = ..scaled..)) +
    geom_density(adjust = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~ trial, nrow = 3) +
    ylab("Density (percentage)") + 
    xlab("Color deviation in degrees")

#Density plot per load per trial per vp
data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec") |>
  ggplot(aes(x = color_angle_deviation, y = ..scaled..)) +
  geom_density(adjust = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ load + trial + url_code) +
  ylab("Density (percentage)") +
  xlab("Color deviation in degrees") +
  theme(strip.text = element_text(size = 8, lineheight = 0.3),  # Adjust the size of the facet labels
        strip.background = element_rect(size = 0.1))



#more plots to have some choice
(prelim_plot <- ggplot(data_all, aes(x = color_angle_abs_deviation, y = trial, color = trial)) +
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


# create overall boxplot for all loads and data points average

#calculate average per participant for geom_jitter points
average_values_abs <- data_all %>%
  dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type != "prom_spec" 
                & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266" ))) |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(average_color_angle_deviation = mean(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg = mean(color_angle_deviation, na.rm = TRUE))

# calculate median per participant
median_values_abs <- data_all %>%
  dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type != "prom_spec" 
                & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(url_code, trial, load, stimulus_type) %>%
  summarise(median_color_angle_deviation = median(color_angle_abs_deviation, na.rm = TRUE),
            overall_avg = mean(color_angle_abs_deviation, na.rm = TRUE))


#create avg value for barplot and for 95% interval
summary_stats_median <- median_values_abs %>%
  dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type != "prom_spec" 
                & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(load, trial) %>%
  summarise(overall_avg = median(median_color_angle_deviation, na.rm = TRUE),
            sd_value = sd(median_color_angle_deviation, na.rm = TRUE),
            ci_lower = overall_avg - qt(0.975, n()) * sd_value / sqrt(n()),
            ci_upper = overall_avg + qt(0.975, n()) * sd_value / sqrt(n()),
            overall_avg = mean(median_color_angle_deviation, na.rm = TRUE))

summary_stats_avg <- average_values_abs %>%
  dplyr::filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type != "prom_spec" 
                & !(url_code %in% c("2712", "4601", "4746", "4788", "4829", "406795254",
                                    "418357214", "733456598", "809438677", "903095408", 
                                    "923811917", "2657", "714802544", "865338266"))) |>
  group_by(load, trial) %>%
  summarise(overall_avg = mean(average_color_angle_deviation, na.rm = TRUE),
            sd_value = sd(average_color_angle_deviation, na.rm = TRUE),
            ci_lower = overall_avg - qt(0.975, n()) * sd_value / sqrt(n()),
            ci_upper = overall_avg + qt(0.975, n()) * sd_value / sqrt(n()))


# change factor key to Baseline and Prospective memory
data_all$trial <- ifelse(data_all$trial == "baseline", "Baseline", 
                         data_all$trial)

data_all$trial <- ifelse(data_all$trial == "baseline2", "Baseline", 
                         data_all$trial)

data_all$trial <- ifelse(data_all$trial == "pm", "Prospective memory", 
                         data_all$trial)


## create main plot with jitter points and 95% interval (Poster Plot!)

color_values <- c("red", "turquoise", "green", "purple", "orange")  # Replace with actual trial colors
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
  scale_x_discrete(labels = c("baseline" = "Baseline", "Prospective memory" = "Prospective \n memory")) +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
  labs(x = "Block type", y = "Average Color Deviation [°]", fill = "Block type", color = "") +
  scale_y_continuous(breaks = seq(0, 50, by = 10)) +
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
#  stat_compare_means(method = "t.test", label = "p.signif", 
#                     aes(group = trial), 
#                     position = position_nudge(x = 0.5))  # Add significance annotations


plot_median <-  plot_median + 
  geom_point(data = summary_stats_median, aes(x = trial, y = overall_avg, color = "Average"), size = 3) +
  scale_color_manual(values = c("black", "red", "blue"), labels = c("Median of all Medians",
                                                                              "Median per person"
                                                                              )) +
  labs(color = "") +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
  theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15)) +
  guides(color = guide_legend(nrow = 2))

print(plot_median)

# save the plot
ggplot2::ggsave(filename = "TeaP_2025_2.pdf", path = "/Users/tobiaskuehlwein/pm_color/TeaP_2025", plot = plot_median, width = 9, height = 7, dpi = 400)



# --------------------------------------------------#
# old version left in for testing
# --------------------------------------------------#

plot_median <- ggplot(data = median_values_abs, aes(x = trial, y = median_color_angle_deviation, fill = trial)) +
                geom_boxplot(alpha = 0.8) +
                geom_jitter(aes(x = trial, y = median_color_angle_deviation, color = trial),
                            show.legend = FALSE, position = position_dodge(width = 0.8)) +
                scale_color_manual(values = color_values) +
                scale_fill_manual(values = color_values) +
                facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
                labs(x = "Block type", y = "Average Color Deviation [°]", fill = "Block type") +
                scale_y_continuous(breaks = seq(0, 50, by = 10)) +
                theme_minimal() +
                theme(strip.text = element_text(size = 12),
                      axis.title.x = element_text(size = 10),  # Adjust x-axis title size
                      axis.text.y = element_text(size = 10),   # Adjust y-axis label size
                      legend.position = "top") +
  stat_compare_means(method = "t.test", label = "p.signif", group = "trial")  # Add significance annotations





plot_median_v2 <-  plot_median_v2 + 
                  geom_point(data = summary_stats_median_v2, aes(x = trial, y = overall_avg, color = "Average"), size = 3) +
                  scale_color_manual(values = c("black", "red", "blue", "violet"), labels = c("Mean",
                                                                                    "Median per person",
                                                                                    "Median per person")) +
                  labs(color = "") +
                  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
                  theme(legend.title = element_text(size = 7), legend.text = element_text(size = 7)) +
                  guides(color = guide_legend(nrow = 2))

plot_median


# --------------------------------------------------#
# End version
# --------------------------------------------------#


# plot for mean instead of median
plot_average <-   ggplot(summary_stats_avg, aes(x = trial, y = overall_avg, fill = trial)) +
                    geom_bar(stat = "identity", position = "identity") +
                    geom_jitter(data = average_values_abs, aes(x = trial, y = average_color_angle_deviation, color = trial),
                                show.legend = FALSE, position = position_dodge(width = 0.8)) +
                    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
                    scale_color_manual(values = c("black", "blue")) +
                    facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable))) +
                    labs(x = "Trial type", y = "Average Color Angle Deviation", fill = "Trial type") +
                    theme_minimal() +
                    theme(legend.position = "right")






# Define a vector of distinct colors
distinct_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                     "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                     "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", 
                     "#c49c94", "#f7b6d2", "#c7c7c7")

# Create the bar plot with distinct colors
bar_plot <- ggplot(data_all, aes(x = trial, y = color_median, fill = url_code)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ url_code + load, labeller = labeller(load = function(variable) paste("Load", variable)), ncol = 6) +
  labs(x = "X Axis Label", y = "Y Axis Label", title = "Bar Plot") +
  theme_minimal() +
  scale_fill_manual(values = distinct_colors)



# create average per VP  per trial per load to use in boxplot for general outliers
load_all_vp_avg_load <- data_all %>%
  group_by(url_code, load, trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation))

avg_split_by_load <- data_all %>%
  group_by(url_code, load, trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation))

# create average per VP to use in boxplot for general outliers
load_all_vp_avg <- data_all %>%
  group_by(url_code, trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation))

#create average oer trial lot split for VP for barplot
load_all_avg <- data_all %>%
  group_by(trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation))



#calculate outliers for naming
outliers_all <- boxplot.stats(load_all_vp_avg$avg_color_angle_abs_deviation)$out

# outliers for plot split by b1l1 to p6l3
outliers_b6l1 <- boxplot.stats(avg_split_by_load$avg_color_angle_abs_deviation)$out

# boxplot for baseline and pm stplit into 6 groups with 20 stimuli each
load_all_base6_pm6_box <- ggplot(load_all_vp_avg, aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_boxplot(alpha = 0.8) + 
  geom_point(data = load_all_vp_avg, aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) +  # Add data points for average values
  labs(x = "Trial", y = "Color Angle Deviation abs") +  # Add axis labels
  theme_minimal()

# add url_code for outliers
load_all_base6_pm6_box <- load_all_base6_pm6_box +
  geom_text(data = load_all_vp_avg[load_all_vp_avg$avg_color_angle_abs_deviation %in% outliers_all, ],
            aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
            vjust = -1)

# barplot for baseline and pm stplit into 6 groups with 20 stimuli each
load_all_base6_pm6_bar <- ggplot(load_all_avg, aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" to plot values directly
  geom_point(data = load_all_vp_avg, aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) +
  labs(x = "Trial load 5", y = "Average Color Angle Deviation abs") +  # Add axis labels
  theme_minimal() 

load_all_base6_pm6_bar <- load_all_base6_pm6_bar +
  geom_text(data = load_all_vp_avg[load_all_vp_avg$avg_color_angle_abs_deviation %in% outliers_all, ],
            aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
            vjust = -1) 

# do the same split by load
load_all_base6_pm6_bar_load <- load_all_vp_avg_load |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  group_by(trial) |>
  summarise(avg_color_angle_abs_deviation = mean(avg_color_angle_abs_deviation)) |>
  ggplot(aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" to plot values directly
  geom_point(data = load_all_vp_avg_load, aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) +
  labs(x = "Trial load 5", y = "Average Color Angle Deviation abs") +  # Add axis labels
  theme_minimal() +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable)))

load_all_base6_pm6_bar_load <- load_all_base6_pm6_bar_load +
  geom_text(data = load_all_vp_avg_load %>%
              dplyr::filter(trial != "" & trial != "Practice") %>%
              dplyr::filter(avg_color_angle_abs_deviation %in% outliers_all),
            aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
            vjust = -1) +
          facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable)))


# do the same as boxplot
load_all_base6_pm6_load_boxplot <- load_all_vp_avg_load |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_boxplot() +  # Use geom_boxplot to create a boxplot
  geom_point(aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) +
  labs(x = "Trial load 5", y = "Average Color Angle Deviation abs") +  # Add axis labels
  theme_minimal() +
  facet_wrap( ~ load, labeller = labeller(load = function(variable) paste("Load", variable)))

load_all_base6_pm6_load_boxplot <- load_all_base6_pm6_load_boxplot +
  geom_text(data = load_all_vp_avg_load %>%
              dplyr::filter(trial != "" & trial != "Practice") %>%
              dplyr::filter(avg_color_angle_abs_deviation %in% outliers_all),
            aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
            vjust = -1) +
  facet_wrap(~ load, labeller = labeller(load = function(variable) paste("Load", variable)))




trial_colors <- c("B1l1"= "red", "B1l2"="red", "B1l3"="red",
                  "B2l1"= "blue", "B2l2"="blue", "B2l3"="blue",
                  "B3l1"="green", "B3l2"="green", "B3l3"="green",
                  "SB4l1"="orange", "SB4l2"="orange", "SB4l3"="orange",
                  "SB5l1"="grey", "SB5l2"="grey", "SB5l3"="grey",
                  "SB6l1"="yellow", "SB6l2"="yellow", "SB6l3"="yellow",
                  "P1l1"="magenta", "P1l2"="magenta", "P1l3"="magenta",
                  "P2l1"="brown", "P2l2"="brown", "P2l3"="brown",
                  "P3l1"="red", "P3l2"="red", "P3l3"="red",
                  "P4l1"="orange", "P4l2"="orange", "P4l3"="orange",
                  "P5l1"="green", "P5l2"="green", "P5l3"="green",
                  "P6l1"="blue", "P6l2"="blue", "P6l3"="blue",
                  "Practice"= "blue")



# change factor key for order
avg_split_by_load$trial <- ifelse(avg_split_by_load$trial == "B6l3", "SB6l3", 
                                  avg_split_by_load$trial)

#reorder the plots so that each trial has the other loads next to it
trial_split <- avg_split_by_load |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = trial, y = avg_color_angle_abs_deviation, fill = trial)) +
  geom_boxplot() +  # Use geom_boxplot to create a boxplot
  geom_point(aes(x = trial, y = avg_color_angle_abs_deviation), color = "black", size = 2) +
  labs(x = "Trial load 5", y = "Average Color Angle Deviation abs") +  # Add axis labels
  scale_fill_manual(values = trial_colors) +  # Manually specify fill colors
  theme_minimal() 

trial_split <- trial_split +
  geom_text(data = avg_split_by_load %>%
              dplyr::filter(trial != "" & trial != "Practice") %>%
              dplyr::filter(avg_color_angle_abs_deviation %in% outliers_b6l1),
            aes(x = trial, y = avg_color_angle_abs_deviation, label = url_code),
            vjust = -1) 

trial_split
trial_split_chron

# save the plot
ggplot2::ggsave(filename = "median_version2.pdf", path = "pm_color/analysis/plots", plot = plot_median_v2, width = 6, height = 4, dpi = 300)

load_all_base6_pm6_load_boxplot


load_all_base6_pm6_box
load_all_base6_pm6_bar
load_all_base6_pm6_bar_load
load_all_base6_pm6_load_boxplot


average_performance_trial <- data_all %>%
                        filter(trial != "" & trial != "practice" & stimulus_type != "prom_spec") %>%
                        group_by(url_code, load, trial) %>%
                        summarise(avg_color_abs_deviation = median(color_angle_abs_deviation))


# ---------------------------------------------------------------
# create line plot for each participant of avg color dev per load
# ---------------------------------------------------------------


# Count the number of unique loads for each url_code
load_counts_trial <- average_performance_trial %>%
  count(url_code)

# Filter out url_code that do not have data for all loads
complete_url_codes_trial <- load_counts_trial %>%
  filter(n == max(load_counts$n)) %>%
  pull(url_code)

# Filter the average_performance data frame to include only complete url_codes
complete_average_performance_trial <- average_performance_trial %>%
  filter(url_code %in% complete_url_codes)


# Convert url_code to factor for coloring
complete_average_performance$url_code <- factor(complete_average_performance$url_code)


# Create the plot
line_plot_vpn <- data_all %>%
                        filter(trial != "" & trial != "practice" & stimulus_type != "prom_spec") %>%
                        ggplot(aes(x = load, y = avg_color_abs_deviation, group = url_code, color = url_code)) +
                        geom_line(data = complete_average_performance) +
                        geom_point(data = complete_average_performance) +
                        labs(x = "Load", y = "Median Color Abs Deviation", title = "Average Median Performance Across Loads") +
                        #facet_wrap(~ trial) + 
                        theme_minimal() +
                        scale_color_hue(l = 50, c = 100) # Adjust lightness and chroma for more distinct colors

line_plot_vpn <- line_plot_vpn +
          geom_line(data = data_all, aes(x = x, y = y, label = label), vjust = -0.5) 

  
line_plot_vpn
line_plot_vpn_trial

# create histogram for avg color dev for each stimulus to see motivation/errors

avg_data <- data_all %>%
  filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" & url_code != "187575071") %>%
  group_by(stimulus_seq, trial) %>%
  summarise(avg_color_angle_abs_deviation = mean(color_angle_abs_deviation, na.rm = TRUE))

histo_stim_seq <- avg_data |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = stimulus_seq, y = avg_color_angle_abs_deviation)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_errorbar(stat = "summary", fun.data = mean_se, width = 0.5) + # Add error bars for variability
  labs(x = "Stimulus Sequence", y = "Average Color Angle Deviation") +
  theme_minimal() +
  facet_wrap(~ trial)

# show plots
plot_median

#histogram for stimulus seq avg color deviation
histo_stim_seq

# line plot showing performance per participant throuout all 3 loads
line_plot_vpn

# save plot to use in poster
ggplot2::ggsave(filename = "median_plot.png", path = "analysis/plots", plot = plot_median, width = 6, height = 6, dpi = 300)

median_plot_nol_not
median_plot_not
plot_average
bar_plot

par(mfrow=c(1,1))

# ----------------------------------------------------

# show all plots for both per load and all together 

# ----------------------------------------------------

#load 1
load_1_base6_pm6_box
load_1_base6_pm6_bar

#load 3
load_3_base6_pm6_box
load_3_base6_pm6_bar

# load 5
load_5_base6_pm6_box
load_5_base6_pm6_bar

# all loads
load_all_base6_pm6_box
load_all_base6_pm6_bar
load_all_base6_pm6_load_boxplot
load_all_base6_pm6_bar_load

# sorted so that base 1 load 1 is next to base 1 load 3, 5 etc.
trial_split
#same plot but sorted in chronological order
trial_split_chron

# general plot for all median to check for overall outliers not split by load or trial
median_plot_nol_not
# Same but split for trial but not load
median_plot_not

#histogram for stimulus seq avg color deviation
histo_stim_seq

# line plot showing performance per participant throughout all 3 loads
line_plot_vpn
line_plot_vpn_trial #(split by trial per vp and not general)

# median plots (poster), with base 1/2 or only base general
plot_median
plot_median_v2

#descriptive data of avg, sd, median, and min, max abs_col_dev for each load and trial 
m_sd_trial_load


#descriptive data of avg, sd, median, and min, max abs_col_dev only load 1
data_l3 |>  
  dplyr::filter(trial != "" & trial != "practice"  & stimulus_type != "prom_spec") |>
  group_by(trial) %>%
  summarise(color_avg = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_median = median(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)

#descriptive data of avg, sd, median, and min, max abs_col_dev for each load, only one trial type
data_all |>  
  dplyr::filter(trial != "" & trial != "practice"  & stimulus_type != "prom_spec"
                & trial != "baseline2" & trial != "baseline1" & url_code != "4746"
                & url_code != "2657") |>
  group_by(trial, load) %>%
  summarise(color_avg = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_median = median(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(load)

# check for sign differences between trial per load

# Filter the data
filtered_data <- data_test |> 
  filter(trial != "" & trial != "Practice" & trial != "practice" & stimulus_type != "prom_spec" & url_code != "4746"
         & url_code != "2657")

# Ensure factors for ANOVA
filtered_data$load <- as.factor(filtered_data$load)
filtered_data$trial <- as.factor(filtered_data$trial)

# Create ANOVA model
anova_model <- aov(color_angle_abs_deviation ~ load * trial, data = filtered_data)

# Perform Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(anova_model)

# View results
print(tukey_results)
