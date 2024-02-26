# Script to analyse data from color pm study
# Created by Tobias Kühlwein

# clear workspace
rm(list=ls())

# choose working direc
setwd("/Users/tobiaskuehlwein/pm_color")


data_all_v4 = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/all_load_test.csv',header=TRUE)

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
describeBy(data_l3$color_angle_deviation, data_l3$trial)
group_vp <- describeBy(data$color_angle_deviation, data$observation)

write.csv(group_vp, file = "data_group_load_1", row.names = TRUE)

ezStats(data$color_angle_abs_deviation, data$trial, within = data$trial_seq)


#create simple ggplot for overwiev
ggplot(data_l3, aes(x = color_angle_deviation, y = trial, color = trial)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA)


VP_data <-   data %>%   # m and sd depending on group in sessions
    group_by(observation, trial) %>%
    summarise(color_dif = mean(color_angle_abs_deviation, na.rm = TRUE),
              color_dif_sd = sd(color_angle_abs_deviation, na.rm = TRUE),
              color_dif_min = min(color_angle_abs_deviation, na.rm = TRUE),
              color_dif_max = max(color_angle_abs_deviation, na.rm = TRUE))

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

# density plit for all
filtered_data |>
  dplyr::filter(trial != "" & trial != "Practice") |>
  ggplot2::ggplot(ggplot2::aes(y = color_angle_deviation, x = trial, group = trial)) +
  ggplot2::facet_wrap(~ load) +
  ggplot2::geom_density() + 
  ggplot2::ggtitle("Density of color deviation by type load 5")

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

density(data$color_angle_abs_deviation, na.rm = TRUE)



#create two boxlpots in one fig
par(mfrow=c(1,2))
boxplot(color_angle_deviation ~ trial, na.rm = TRUE, data = data_l5)
boxplot(T2 ~ Condition, data = DataE2)

par(mfrow=c(1,1))

