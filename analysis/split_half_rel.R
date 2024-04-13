## Script for reliability calculation
#Written by Tobias Kühlwein

#read in data
histo_all = read.csv("/Users/tobiaskuehlwein/pm_color/analysis/all_load_test.csv", header = TRUE)

#load package
library("splithalf")
library("dplyr")

# load all: histo_all
# load 1: data_l1
# load 3: subset_data_l2 (add & url_code != 541338211) to exclude missing data
# load 5: subset_data_l5


#Working main calculations
data_l_all <- histo_all |>
  dplyr::filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" 
        & !is.na(color_angle_abs_deviation) & ! is.na(url_code)) |>
  dplyr::mutate(load_condition = paste(load, trial, sep = "_")) # |>
  # dplyr::group_by(url_code, load_condition) |>
  # dplyr::tally()

unique(data_l_all$load_condition)
splithalf(data = data_l_all,
          outcome = "accuracy",
          score = "average",
          conditionlist = unique(data_l_all$load_condition),
          halftype = "random",
          permutations = 5000,
          var.ACC = "color_angle_abs_deviation",
          var.condition = "load_condition",
          var.participant = "url_code",
          average = "mean",
          plot = TRUE,
          round.to = 2,
          check = TRUE
)


#check oddeven reliability by hand/bootstrap
data_l1 |>
  filter(trial != "" & trial != "Practice" & stimulus_type != "prom_spec" 
         & !is.na(color_angle_abs_deviation) & trial == "Baseline") |>
  # dplyr::group_by(url_code) |>
  dplyr::mutate(odd = dplyr::if_else(dplyr::row_number() %% 2 == 1, "odd", "even")) |>
  dplyr::group_by(url_code, odd) |>
  dplyr::summarise(mean = mean(color_angle_abs_deviation)) |>
  tidyr::pivot_wider(id_cols = "url_code", values_from = "mean", names_from = "odd") -> d

cor.test(d$odd, d$even)

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

