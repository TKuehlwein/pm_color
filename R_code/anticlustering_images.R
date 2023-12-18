rm(list=ls())

# load in packages
library(tidyverse)
library(anticlust)
library(readxl)


getwd()
# set working directory
setwd("/Users/tobiaskuehlwein/pm_color/stim_lists/BOSS")

# read in excel file
stimuli_norms_2014a <- read_excel("BOSS_NORMS_December15_2014.xlsx", sheet = "Brodeur et al 2014a")
# stimuli_norms_2014b <- read_excel("BOSS_NORMS_December15_2014.xlsx", sheet = "Brodeur et al 2014b")
 
# choose relevant columns
stimuli_norms_2014a <- stimuli_norms_2014a %>%
  select(one_of(c("FILENAME", "Mean...31", "StDev...32", "Mean...35",
                  "StDev...36"))) %>%
  set_names(c("filename", "familiarity_mean_2014a",
              "familiarity_sd_2014a", "visual_complexity_mean_2014a", "visual_complexity_sd_2014a"))

# stimuli_norms_2014b <- stimuli_norms_2014b %>%
#   select(one_of(c("FILENAME...10", "Mean...15", "StDev...16"))) %>%
#   set_names(c("filename", "visual_complexity_mean_2014b", "visual_complexity_sd_2014b"))

# read in stimulus list
setwd("/Users/chhavisachdeva/Documents/Documents - MAC-J77PF9K56N/Postdoc/Stimuli/Stimuli_lists")

stimuli_list <- read.csv("stimuli_list_v2.csv", header = TRUE)

stimuli_list <- subset(stimuli_list, select = -X)

# remove the .png so the name of the stimuli are consistent between dataframes
stimuli_list <- stimuli_list %>%
  mutate(filename = tools::file_path_sans_ext(filename))

# include only stimuli from stimuli_list to stimuli_norms_2014a
stimuli_norms_2014a <- stimuli_norms_2014a %>%
  filter(filename %in% stimuli_list$filename)

# see which stimuli is missing

# extract the filenames from both dataframes
filenames_norms <- stimuli_norms_2014a$filename
filenames_list <- stimuli_list$filename

# find the missing filenames
missing_filenames <- setdiff(filenames_list, filenames_norms)

# print the missing filenames
print(missing_filenames) # dvd case is missing

# remove images with missing values
stimuli_norms_2014a_NA_remove <- na.omit(stimuli_norms_2014a)

#seed first three lists
#set.seed(123)

#seed last three lists
set.seed(132)

#reset seed
set.seed(NULL)


# anticluster for the different tasks
anticluster_stimuli <- anticlustering(
  stimuli_norms_2014a_NA_remove[, -1],
  K = 3,
  objective = 'diversity',
  method = 'local-maximum',
  repetitions = 10,
  categories = stimuli_norms_2014a_NA_remove$filename
)


table(anticluster_stimuli)

knitr::kable(mean_sd_tab(stimuli_norms_2014a_NA_remove[, -1], anticluster_stimuli), row.names = TRUE)


stimuli_allocation <- as.data.frame(table(anticluster_stimuli, stimuli_norms_2014a_NA_remove$filename))

stimuli_allocation$task <- ifelse(stimuli_allocation$anticluster_stimuli == 1 & stimuli_allocation$Freq == 1, "1",
                 ifelse(stimuli_allocation$anticluster_stimuli == 2 & stimuli_allocation$Freq == 1, "2",
                        ifelse(stimuli_allocation$anticluster_stimuli == 3 & stimuli_allocation$Freq == 1, "3",
                               ifelse(stimuli_allocation$anticluster_stimuli == 4 & stimuli_allocation$Freq == 1, "4",
                                      ifelse(stimuli_allocation$anticluster_stimuli == 5 & stimuli_allocation$Freq == 1, "5", "6")))))
  
stimuli_allocation <- stimuli_allocation[!(stimuli_allocation$Freq == 0),] 


summary(stimuli_allocation)

write.csv(stimuli_allocation, file='/Users/tobiaskuehlwein/pm_color/stim_lists/BOSS/list_4_6.csv', 
          fileEncoding = "UTF-8")

