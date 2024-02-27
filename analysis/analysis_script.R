# Script to analyse data from color pm study
# Created by Tobias Kühlwein

# clear workspace
rm(list=ls())

# choose working direc
setwd("/Users/tobiaskuehlwein/pm_color")


data_all = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_all.csv',header=TRUE)
data_l1 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_1_v2.csv',header=TRUE)
data_l3 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_3.csv',header=TRUE)
data_l5 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/load_5.csv',header=TRUE)

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

#create simple boxplot for overwiev
boxplot(data$color_angle_abs_deviation, xlab = "trial", ylab="Points rated strictly", main="Correctly remembered words for each condition in session two")

#create simple plot for overwiev
plot(data_all_v4$color_angle_abs_deviation, data_all_v4$trial, xlab = "deviation" ,
     ylab="Trial" , col=as.factor(data_all_v4$trial), pch=19, na.rm = TRUE)
      legend("topleft",
       legend = levels(factor(data_all_v4$trial)),
       pch = 19,
       col = factor(levels(factor(data_all_v4$trial))))
#abline(lm(Data$Sitzung1 ~ Data$Sitzung2), col = 4, lwd = 3)
#Regression_Data <- lm(Data$Sitzung1 ~ Data$Sitzung2 )

#get description based on session

  #dplyr::filter(participant_id != 402695612) |>
describeBy(data_all$color_angle_abs_deviation, data_all$load)

group_vp <- describeBy(data_all$color_angle_deviation, data_all$load)

write.csv(group_vp, file = "data_group_load_1", row.names = TRUE)

ezStats(data$color_angle_abs_deviation, data$trial, within = data$participant_id)


#create simple ggplot for overwiev
ggplot(data_l3, aes(x = color_angle_deviation, y = trial, color = trial)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA)

# m and sd for each trial type per load
data_all |>  
  dplyr::filter(trial != "" & trial != "Practice") |>
  group_by(trial, load) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(trial)

# m, sd, min, max for each trial per load per vp
data_l5 |>  
  dplyr::filter(trial != "" & trial != "Practice") |>
  group_by(trial, url_code) %>%
  summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
            color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE),)%>%
  arrange(trial)

mean(data$color_angle_abs_deviation, na.rm = TRUE)

summary(data_l3$color_angle_deviation, na.rm = TRUE)


#rename col if needed
colnames(data)
names(df2)[names(df2) == "Bedingung"] <- "Condition"

# show potential outliers
sum(data_all_v4$color_angle_abs_deviation, na.rm = TRUE)
count_within_range <- sum(data_all_v4$color_angle_abs_deviation >= 100, na.rm = TRUE)
print(count_within_range)

# filter out potential outliers
filtered_data <- subset(data_all_v4, !(color_angle_abs_deviation >= 100))


#first LME 
LME_l1 <- lmer(color_angle_abs_deviation ~ trial + load + stimulus_type + (1|trial_type),
              data = data, na.action = na.exclude)


#check LME and create some simple plots for them
summary(LME_1)
plot(LME_1, 1)
Anova(LME6, type = "3")
summary(LME) # 12 % Der Varianz durch Sitzung 1 erklärt (Varianz Sitzung 1 / gesamtvarianz (also Plus Residual))
pred.mmE2 <- ggpredict(LMEE2, terms = c("Condition"))

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
  ggplot2::ggplot(ggplot2::aes(y = color_angle_deviation, x = trial, group = trial)) +
  ggplot2::facet_wrap(~ load + trial) +
  ggplot2::geom_violin() + 
  ggplot2::ggtitle("Density of color deviation by type load 5")

data_load1 |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation, fill = participant_id)) + 
  geom_histogram( bins = 180) +
  facet_wrap(~ participant_id)



data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  ggplot(aes(x = color_angle_deviation, colour = trial)) + 
    geom_histogram(bins = 60) + 
  facet_wrap(~ load)

data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  ggplot(aes(x = color_angle_deviation)) +
    geom_density() +
  ggplot2::facet_wrap(~ load )
  

data_all |>
  dplyr::filter(trial != "" & trial != "Practice" & participant_id != 402695612) |>
  ggplot(aes(x = color_angle_deviation)) + 
    geom_histogram(aes(y = ..density.., bins = 100),
                   colour = 1, fill = "white") +
    geom_density() +
    facet_wrap(~ trial + load, nrow = 3)
    

## Some useful plots


# density plot per load per trial
data_all |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation)) + 
  geom_freqpoly(bins = 180) +
  facet_wrap(~ load + trial, nrow = 3)

#different density plot
data_all |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot(aes(x = color_angle_deviation, y = ..scaled..)) +
    geom_density(adjust = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~ load + trial, nrow = 3) +
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


#some requirement testing
qqnorm(resid(LMEE2))
qqline(resid(LMEE2))
plot(lm(color_angle_abs_deviation ~ trial, data = data_all_v4))


#more plots to have some choice
(prelim_plot <- ggplot(data, aes(x = color_angle_abs_deviation, y = trial, color = trial)) +
    geom_point() +
    geom_smooth(color = "Red" ,method = "lm"))

(split_plot <- ggplot(aes(color_angle_deviation, trial), color = red, data = data) + 
    geom_point() + 
    facet_wrap(~ trial_seq) + 
    xlab("Trial1") + 
    ylab("Trial2"))


g<-ggplot(data,aes(x=color_angle_deviation,y=trial,fill=trial_seq))+stat_boxplot(geom="errorbar")+geom_boxplot()+theme_bw()
(g+labs(x="",y="Measurement (units)")+coord_cartesian(ylim=c(0,20))+scale_y_continuous(breaks=seq(0,300,50))+
    scale_x_discrete(breaks=c("Time01","Time02","Time03"),labels=c("GroupA","GroupB","GroupC"))+
    theme(text = element_text(size=15),legend.title = element_blank(),legend.position="bottom"))
g

modelE22 <- lm(T2 ~ T1, data=DataE2) # Berechnung Standartresidual
summary(modelR1)
lm(formula = Session1 ~ Session2, data = Data_gesamt)
standard_res <- rstandard(modelR1)
standard_res

geom_density(data$color_angle_abs_deviation, na.rm = TRUE)



#create two boxlpots in one fig
par(mfrow=c(1,2))
boxplot(color_angle_deviation ~ trial, na.rm = TRUE, data = data_l5)
boxplot(T2 ~ Condition, data = DataE2)

par(mfrow=c(1,1))

