# ==============================================================================
#
# This code was developed by Nicolas Rothen.
#
# The purpose of the code is to visualize the characteristics of the different
# stimulus lists.


# ================================================================= settings ===

# required R packages
library(ggpubr)
library(tidyverse)

setwd("/Users/tobiaskuehlwein/pm_color/stim_lists")

# clear workspace
rm(list=ls())


# settings
dir_input  <- paste0(getwd(), "/new_lists/pm/")
dir_output <- paste0(getwd(), "/new_lists/pm/")

# avoid scientific notation
options(scipen = 100)

# set seed for reproducibility
set.seed(20221213)

# re-range hue from 0 to 360 (add the following value)
hue_correction <- 179


# ================================================================ read data ===

# list all files in folder
list_of_files <- list.files(path = dir_input, recursive = TRUE,
                            pattern = "\\.csv$", full.names = TRUE)

# bind data of all files into a single tibble
data <- read_csv(list_of_files, id = "file_name") %>%
  mutate(file_name = basename(file_name)) %>%
  mutate(load = as.numeric(str_extract_all(file_name, "\\d{1}(?=.csv)")))


# =========================================================== data wrangling ===

hues <- data %>%
  select(block_number, stimulus_seq, trial_number, stimulus_type,
         load, hue_v1, hue_v2, hue_prom_v1, hue_prom_v2) %>%
  pivot_longer(!block_number & !stimulus_seq & !trial_number & !stimulus_type & !load,
               names_to = "condition", values_to = "hue") %>%
  filter(!is.na(hue)) %>%
  mutate(hue = hue + hue_correction, load = paste0("load ", load),
         version = paste0("version ", parse_number(condition)))


# =========================================== visualize distribution of hues ===

desc <- hues %>%
  summarise(avg = mean(hue), min = min(hue), max = max(hue))

hues %>%
  ggplot(aes(x = hue)) +
  geom_histogram(binwidth = 1) +
  theme_pubclean(base_size = 20) +
  ggtitle("distribution of hues across all conditions")

plt_col_dist <-
hues %>%
  ggplot(aes(x = hue, y = "", color = hue)) +
  geom_point() +
  scale_colour_gradientn(colours = rainbow(360)) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 45)) +
  coord_polar() +
  facet_grid(version ~ load, switch = "x") +
  theme_minimal(base_size = 15) +
  labs(x = "", y = "") +
  ggtitle("Color distribution") +
  theme(strip.background = element_rect(color = "black", fill = "grey90", linetype = "blank"))

plt_load_1 <-
hues %>%
  filter(load == "load 1", block_number > 0) %>%
  ggplot(aes(x = trial_number, y = hue, color = hue, shape = stimulus_type)) +
  geom_line(aes(group = trial_number), color = "grey") +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = rainbow(360)) +
  scale_shape_manual(values =c(1, 15)) +
  theme_minimal(base_size = 15) +
  facet_grid(version ~ .) +
  scale_y_continuous(breaks = c(0, 360, 90, 180, 270)) +
  ggtitle("Load 1") +
  labs(x = "", y = "") +
  theme(strip.background = element_rect(color = "black", fill = "grey90", linetype = "blank"),
        panel.grid.minor.y = element_blank())

plt_load_3 <-
hues %>%
  filter(load == "load 3", block_number > 0) %>%
  ggplot(aes(x = trial_number, y = hue, color = hue, shape = stimulus_type)) +
  geom_line(aes(group = trial_number), color = "grey") +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = rainbow(360)) +
  scale_shape_manual(values =c(1, 15)) +
  theme_minimal(base_size = 15) +
  facet_grid(version ~ .) +
  scale_y_continuous(breaks = c(0, 360, 90, 180, 270)) +
  ggtitle("Load 3") +
  labs(x = "", y = "") +
  theme(strip.background = element_rect(color = "black", fill = "grey90", linetype = "blank"),
        panel.grid.minor.y = element_blank())

plt_load_5 <-
hues %>%
  filter(load == "load 5", block_number > 0) %>%
  ggplot(aes(x = trial_number, y = hue, color = hue, shape = stimulus_type)) +
  geom_line(aes(group = trial_number), color = "grey") +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = rainbow(360)) +
  scale_shape_manual(values =c(1, 15)) +
  theme_minimal(base_size = 15) +
  facet_grid(version ~ .) +
  scale_y_continuous(breaks = c(0, 360, 90, 180, 270)) +
  ggtitle("Load 5") +
  labs(x = "", y = "") +
  theme(strip.background = element_rect(color = "black", fill = "grey90", linetype = "blank"),
        panel.grid.minor.y = element_blank())




plt_all <- ggarrange(plt_load_1, plt_load_3, plt_load_5,
                     ncol = 1, nrow = 3, labels = "AUTO", common.legend = TRUE,
                     legend = "right", heights = c(1, 1, 1))


# =============================================================== save plots ===

ggsave(filename = paste0(dir_output, "color_distribution.pdf"),
       plot = plt_col_dist, width = 29.7, height = 21.0, units = "cm")

ggsave(filename = paste0(dir_output, "color_trial_conditions.pdf"),
       plot = plt_all, width = 21, height = 29.7, units = "cm")


# ====================================================================== END ===

