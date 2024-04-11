## Script for reliability calculation
#Written by Tobias Kühlwein

#read in data
histo_all = read.csv("/Users/tobiaskuehlwein/pm_color/analysis/all_load_test.csv", header = TRUE)

#load package
library("splithalf")
library("dplyr")


#Working main calculations
splithalf(data = histo_all %>%
          filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" & !is.na(color_angle_abs_deviation)),
          outcome = "accuracy",
          score = "average",
          conditionlist = c("Baseline", "Pm"),
          halftype = "random",
          permutations = 5000,
          var.ACC = "color_angle_abs_deviation",
          var.condition = "trial",
          var.participant = "url_code",
          average = "mean",
          plot = FALSE,
          round.to = 2,
          check = TRUE
)



#create a better way of spliting the condions
split_half_data <- histo_all %>%
  mutate(rel_cond = paste(trial, load, sep = " "))


# Run splithalf function on dataset (not working for split condition)
# splithalf(data = split_half_data %>%
#           filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" & !is.na(color_angle_abs_deviation)),
#           outcome = "accuracy",
#           score = "average",
#           conditionlist = c("Baseline 1", "Pm 1", 
#                             "Baseline 3", "Pm 3", 
#                             "Baseline 5", "Pm 5"),
#           halftype = "random",
#           permutations = 5000,
#           var.ACC = "color_angle_abs_deviation",
#           var.condition = "rel_cond",
#           var.participant = "participant_id",
#           average = "mean",
#           plot = FALSE,
#           round.to = 2,
#           check = TRUE
# )

