# clear console
cat('\014')

# clear workspace/except for simulated data in second line
rm(list = ls())
rm(list = setdiff(ls(), "simulation"))

# load in required R packages
library(tidyverse)
library(simr)
library(mixedpower)
library(report)
library(jtools)
library(lsr)
library(truncnorm)

# set working directory
setwd("/Users/tobiaskuehlwein/pm_color/Downloads")

# load in pilot data file
df_long <- read.csv("dataframe_output_v3.csv", header = TRUE)

# reset working directory
setwd("/Users/chhavisachdeva/Documents/Documents - MAC-J77PF9K56N/Postdoc/Chhavi/Final_task_versions_pilot/STM_pilot/Data/power_simulation")

# set seed
seed <- set.seed(61223)

# turn of scientific numbering
options(scipen = 999)

# create a dataframe with participant IDs
participantID <- 1:200

################## LOAD 1

# add trial information 
load1 <- expand.grid(participant_id = participantID, trial = 1:2)

# order dataframe by participant ID
load1 <- load1[order(load1$participant_id),]

# create counterbalancing variable
counterbalancing <- data.frame(participant_id = unique(load1$participant_id),
                               counterbalancing = sample(1:2, 50, replace = TRUE))
load1 <- merge(load1, counterbalancing, by = "participant_id")

# check mean and sds
mean_spatial_control <- median(data_all[data_all$color_angle_abs_deviation, 'color_angle_abs_deviation'])

mad_color <- data_all %>%
  filter(trial != "practice") %>%  # Exclude 'practice' trial
  group_by(load, trial) %>%
  summarise(mad_color_angle_abs_deviation = mad(color_angle_abs_deviation, na.rm = TRUE))

median_color <- data_all %>%
  filter(trial != "practice") %>%  # Exclude 'practice' trial
  group_by(load, trial) %>%
  summarise(median_color_angle_abs_deviation = median(color_angle_abs_deviation, na.rm = TRUE))

mean_color_control <- mean(results[results$load == 1, 'color_angle_abs_deviation'])

sd_color_control <- mad(results[results$load == 1, 'color_angle_abs_deviation'])

# cohen's d effect size
d <- 0.6

# calculate the mean difference for the two conditions
mean_color_diff_l1_base <- d * median_color_l1_base
mad_color_diff <- d * mad_color$mad_color_angle_abs_deviation

# adjust mean for the experimental group
mean_color_exp <- median_color$median_color_angle_abs_deviation - mean_color_diff
mad_color_exp <- mad_color$mad_color_angle_abs_deviation - mad_color_diff


#rename colum and variables
names(simulation)[names(simulation) == "participant_id"] <- "participant"
simulation$trial <- ifelse(simulation$trial == "2", "pm", 
                           simulation$trial)
# introduce the effects based on Cohen's d of 0.6 for each condition to add to dataframe
simulation <- simulation %>%
  mutate(color_abs_deviation = ifelse(load == 1 & trial == "base",
                                      rtruncnorm(n(), a = 0, b = 179, mean = median_color_l1_base, sd = mad_color_l1_base),
                                      ifelse(load == 1 & trial == "pm",
                                             rtruncnorm(n(), a = 0, b = 179, mean = median_color_l1_pm, sd = mad_color_l1_pm),
                                             ifelse(load == 3 & trial == "base",
                                                    rtruncnorm(n(), a = 0, b = 179, mean = median_color_l3_base, sd = mad_color_l3_base),
                                                    ifelse(load == 3 & trial == "pm",
                                                           rtruncnorm(n(), a = 0, b = 179, mean = median_color_l3_pm, sd = mad_color_l3_pm),
                                                           ifelse(load == 5 & trial == "base",
                                                                  rtruncnorm(n(), a = 0, b = 179, mean = median_color_l5_base, sd = mad_color_l5_base),
                                                                  ifelse(load == 5 & trial == "pm",
                                                                         rtruncnorm(n(), a = 0, b = 179, mean = median_color_l5_pm, sd = mad_color_l5_pm),
                                                                         NA)))))))
#seperate data_long into two colums for trial and load

df_separated <- df_long %>%
  separate(condition, into = c("load", "trial"), sep = 3)

#ensure the data is correct and makes sense
df_test   <-  df_separated |>  
  group_by(trial, load) %>%
  summarise(color_avg = mean(value, na.rm = TRUE),
            color_median = median(value, na.rm = TRUE),
            color_dif_sd = sd(value, na.rm = TRUE),
            color_dif_min = min(value, na.rm = TRUE),
            color_dif_max = max(value, na.rm = TRUE),)%>%
  arrange(load)

view(sim_test)


# check for differences between groups 
results <- NULL
for (t in c("base", "pm")) {
  loads <- list(c(1,3), c(1,5), c(3,5))
  for (i in 1:3) {
    results <- results |>
      dplyr::bind_rows(t.test(color_abs_deviation ~ load,
         data = dplyr::filter(simulation, trial == t & load %in% loads[[i]])) |>
           broom::tidy() |>
           dplyr::mutate(trial = t,
                         loads = paste(loads[[i]], collapse = ", "),
                         .before = 0))
  }
}




# turn variables in to factors
simulation$load <- as.factor(simulation$load)
simulation$trial <- as.factor(simulation$trial)
#simulation$participant <- as.factor(simulation$participant)
# artificial_data$load <- as.factor(artificial_data$load)

# run t-test ot check for interaction between load 3 and trial
load3 <- subset(simulation, load == 3)

t.test(color_abs_deviation ~ trial, paired = TRUE,  load3)
cohens_d(load3, color_abs_deviation ~ trial, paired = TRUE)

# # run t-tests to check data for color.cohen <- 
# color_recall <- subset(artificial_data, recall == "Color" & load == 1)
# 
# t.test(deviation ~ domain, simulation)
# cohensD(deviation ~ domain, color_recall)
# 
# color_recall <- subset(artificial_data, recall == "Color" & load == 3)
# 
# t.test(load ~ d, color_recall)
# cohensD(deviation ~ domain, color_recall)
# 
# color_recall <- subset(artificial_data, recall == "Color" & load == 5)
# 
# t.test(deviation ~ domain, color_recall)
# cohensD(deviation ~ domain, color_recall)
# 
# # run t-tests to check data for location
# spatial_recall <- subset(artificial_data, recall == "Spatial" & load == 1)
# 
# t.test(deviation ~ domain, spatial_recall)
# cohensD(deviation ~ domain, spatial_recall)
# 
# spatial_recall <- subset(artificial_data, recall == "Spatial" & load == 3)
# 
# t.test(deviation ~ domain, spatial_recall)
# cohensD(deviation ~ domain, spatial_recall)
# 
# spatial_recall <- subset(artificial_data, recall == "Spatial" & load == 5)
# 
# t.test(deviation ~ domain, spatial_recall)
# cohensD(deviation ~ domain, spatial_recall)

# figure out contrasts
contrasts(simulation$trial)
viewcontrasts(simulation$trial)
artificial_data$stimulus_feature <- as.factor(artificial_data$stimulus_feature)
contrasts(simulation$load)
# contrasts(artificial_data$load)

# define model
fit <- lmer(value ~ load * trial + (1|participant)
              , data = df_separated)
summary(fit)
anova(fit)
summ(fit)
parameters::p_value(fit)
sigma(fit)

# figure out contrasts for pilot data
data_all$trial <- factor(data_all$trial)
levels(data_all$trial)
contrasts(data_all$trial)

# manually set contrast levels for pilot data
contrast_matrix <- matrix(c(0, 1), ncol = 1)

# apply the contrast to the trial_type variable in pilot data
contrasts(results$trial_type) <- contrast_matrix

# verify the contrasts for pilot data
contrasts(results$trial_type)

# check pilot model for participant variance
fit_pilot <- lmer(color_angle_abs_deviation ~ load * trial + (1|url_code), 
                  data = data_all, 
                  subset = (trial != "practice"))
summary(fit_pilot)
summ(fit_pilot)
summary(fit_pilot)

# information about the model used for simulation
model <- fit
data <- df_long
fixed_effects <- fixef(fit)
random_variance <- VarCorr(fit)
sigma <- sigma(fit)

# make artificial model
artificial_lmer <- makeLmer(formula = value ~ load * trial + (1|participant), fixef = fixed_effects, 
                            VarCorr = random_variance, sigma = sigma, data = df_separated)
summary(artificial_lmer)
anova(artificial_lmer)

set.seed(234)

# simulation parameters
steps <- c(160,170,190,210,250,280,300)
critical_value <- 2
n_sim <- 500
SESOI <- fixed_effects
fixed_effects <- c("load", "trial")

# change pptid to numeric
df_separated$participant <- as.numeric(df_separated$participant, na.RM = TRUE)

# run simulation
power5 <- mixedpower(model = artificial_lmer, data = df_separated,
                     fixed_effects = fixed_effects, simvar = "participant", steps = steps,
                     critical_value = critical_value, n_sim = n_sim, SESOI = SESOI)

#using dataframe created by me
power3
#my dataframe but different interaction
power5
#using dataframe by chaning brys code
power4
multiplotPower(power5)

#dataframe simulated by me
df_test
#dataframe by changing brysbaert code
sim_test

#check stuff by hand
load3_br <- subset(df_hi, load == "l3_")
t.test(datalong ~ trial, paired = TRUE,  load3_br)
cohens_d(load3_br, datalong ~ trial, paired = TRUE)


# save power analysis
write.csv(power3, file = "power_pm_90%_pure.csv", row.names = TRUE)


df_separated <- df_long %>%
     separate(conds, into = c("load", "trial"), sep = 2)

