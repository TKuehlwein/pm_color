# Script to analyse data from color pm study
# Created by Tobias Kühlwein

# clear workspace
rm(list=ls())

# choose working direc
setwd("/Users/tobiaskuehlwein/pm_color")


data = read.csv('/Users/tobiaskuehlwein/pm_color/analysis/data_test.csv',header=TRUE)

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
boxplot(data, xlab = "Condition", ylab="Points rated strictly", main="Correctly remembered words for each condition in session two")

#create simple plot for overwiev
plot(data$color_angle_abs_deviation, data$trial_type, xlab = "Session 1" ,
     ylab="Session 2" , col=as.factor(DataE2$trial_type), pch=19)
legend("topleft",
       legend = levels(factor(DataE2$Condition)),
       pch = 19,
       col = factor(levels(factor(DataE2$Condition))))
#abline(lm(Data$Sitzung1 ~ Data$Sitzung2), col = 4, lwd = 3)
#Regression_Data <- lm(Data$Sitzung1 ~ Data$Sitzung2 )

#get description based on session
describeBy(DataListe$Sitzung1, DataListe$Sitzung2)
ezStats(DataE2$T2, DataE2$T1, within = DataE2$Condition)


#create simple ggplot for overwiev
ggplot(data, aes(x = color_angle_abs_deviation, y = trial_type, color = practice)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA)


data %>%   # m and sd depending on group in sessions
  group_by(trial_type) %>%
  summarise(color_dif = mean(color_angle_abs_deviation),
            color_dif_sd = sd(color_angle_abs_deviation),
            mean_Trial2 = mean(trial_number),
            sd_Trial2 = sd(trial_number))

summary(data$color_angle_abs_deviation, na.rm = TRUE)


#rename col if needed
colnames(df2)
names(df2)[names(df2) == "Bedingung"] <- "Condition"

#first LME 
mixed.lmerE3 <- lmer(T2 ~ Condition + T2 + know + ListS1 + (1|major) + (1|Ss), data = dfE3, na.action = na.exclude)
LME6 <- lmer(color_angle_abs_deviation ~ trial_type + (1|trial_seq), data = data, na.action = na.exclude,
             control=lmerControl(check.nobs.vs.nlev = "ignore",
                                 check.nobs.vs.rankZ = "ignore",
                                 check.nobs.vs.nRE="ignore"))

#check LME and create some simple plots for them
summary(LME6)
plot(LMEE3, 1)
Anova(LME6, type = "3")
summary(LMEE3) # 12 % Der Varianz durch Sitzung 1 erklärt (Varianz Sitzung 1 / gesamtvarianz (also Plus Residual))
plot(LMEE2)
pred.mmE2 <- ggpredict(LMEE2, terms = c("Condition"))
summary(pred.mmE2)
anova(LME6)

summary(LME6)

#create table to be used in paper two variants
sjPlot::tab_model(LME6, 
                  show.re.var= TRUE,
                  dv.labels= "Effects on points in session 2")

stargazer(LME6, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

#some requirement testing
qqnorm(resid(LMEE2))
qqline(resid(LMEE2))
plot(lm(T1 ~ T2, data = DataE3))



#more plots to have some choice
(prelim_plot <- ggplot(DataE2, aes(x = T1, y = T2, color = Condition)) +
    geom_point() +
    geom_smooth(color = "Red" ,method = "lm"))

(split_plot <- ggplot(aes(T1, T2), color = red, data = DataE2) + 
    geom_point() + 
    facet_wrap(~ Condition) + 
    xlab("Trial1") + 
    ylab("Trial2"))


g<-ggplot(DataE2,aes(x=T1,y=T2,fill=Condition))+stat_boxplot(geom="errorbar")+geom_boxplot()+theme_bw()
(g+labs(x="",y="Measurement (units)")+coord_cartesian(ylim=c(0,20))+scale_y_continuous(breaks=seq(0,300,50))+
    scale_x_discrete(breaks=c("Time01","Time02","Time03"),labels=c("GroupA","GroupB","GroupC"))+
    theme(text = element_text(size=15),legend.title = element_blank(),legend.position="bottom"))
g

modelE22 <- lm(T2 ~ T1, data=DataE2) # Berechnung Standartresidual
summary(modelR1)
lm(formula = Session1 ~ Session2, data = Data_gesamt)
standard_res <- rstandard(modelR1)
standard_res

#create two boxlpots in one fig
par(mfrow=c(1,2))
boxplot(color_angle_abs_deviation ~ trial_type, na.rm = TRUE, data = data)
boxplot(T2 ~ Condition, data = DataE2)

par(mfrow=c(1,1))

