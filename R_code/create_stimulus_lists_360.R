# ==============================================================================
#
# This code was developed by Nicolas Rothen.
#
# The purpose of the code is to create the stimulus lists for a prospective
# memory task with a visual working memory version of the delayed estimation
# task.


# ================================================================= settings ===

# required R packages
library(tidyverse)

# settings
dir_stimuli       <- paste0(getwd(), "/stimulus_lists/images")
dir_output        <- paste0(getwd(), "/stimulus_lists/output")
seed              <- set.seed(20220513)
n_stimuli         <- 360
n_practice_trials <- 3
min_distance      <- 40 # minimal distance between colors in degrees (max distance is twice min distance)
pm_cat            <- c('prm_505.jpg')
pm_cues           <- c('prm_501.jpg', 'prm_502.jpg', 'prm_503.jpg', 'prm_504.jpg')

# experimental conditions
block <- c("baseline", "prom")
load  <- c(1, 3, 5)            # images are repeated between load conditions

# perform some basic checks before building the stimulus lists
n_pm    <- length(pm_cues)
n_cat   <- length(pm_cat)
n_ong   <- n_stimuli + max(load * n_practice_trials) + (max(load) - 1) * n_pm
stimuli <- list.files(path = dir_stimuli, pattern = "*.jpg", full.names = F)
n_cond  <- length(block) * n_pm
n_stimuli_per_cond <- length(n_stimuli) / n_cond

if(n_stimuli %% n_cond != 0){
  stop("Number of stimuli NOT DEVISABLE by experimental conditions")
}

if(if_else(n_stimuli_per_cond %% load == 0, 1, 0) %>%
   sum() %% length(load) != 0){
  stop("Stimuli per condition NOT DEVISABLE by load of each load condition")
}

# are there enough stimuli to build all the lists?
if((length(stimuli) - (n_pm + n_cat)) < n_stimuli){
  stop("There are NOT ENOUGH stimuli available in your image directory")
}

# ======================================================== compile functions ===

n_colors <- function(load) {
  # determine maximum number of colors for any given trial
  return(max(load) + 1)
}

equidistant_colors <- function(n_colors){
  # determine equidistant colors on circle
  return(seq(0, 360-(360/n_colors), 360/n_colors))
}

jitter_range <- function(n_colors, min_distance) {
  # determine range in degrees within individual colors of a trial can vary
  return(0:(360 / n_colors - min_distance))
}

random_colors <- function(n_colors, min_distance) {
  # create random colors, keeping a defined minimum distance between colors
  eq_cols <- equidistant_colors(n_colors)
  j_range <- jitter_range(n_colors, min_distance)
  j_cols  <- equidistant_colors(n_colors) + sample(j_range, n_colors)
  return(list(j_cols + sample(0:(min_distance-1), n_colors)))
  #return(list(sample(j_cols + sample(0:(min_distance-1), 1))))
}

color_dev_deg <- function(target, response) {
  return((((target - response) + 180) %% 360) - 180)
}

# ================================================ create folders for output ===

if (file.exists(dir_output)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(dir_output)
  
}


# ================================================= create basic image lists ===

for (load_i in 1:length(load)) {
  
  current_load <- load[load_i]
  
  stim_all <- stimuli %>%
    as_tibble() %>%
    rename(image = value) %>%
    add_column(block_number = NaN, stimulus_seq = NaN, trial_seq = NaN,
               trial_type = "", trial_number = NaN, hue = NaN) %>%
    mutate(stimulus_type = if_else(image %in% pm_cues, "prom_spec", "ongoing")) %>%
    mutate(stimulus_type = if_else(image %in% pm_cat, "prom_cat", stimulus_type)) %>%
    relocate(image, hue, .after = trial_number) %>%
    relocate(trial_type, stimulus_type, .before = image)
  
  stim_remaining <- stim_all
  
  stim_cat <- stim_remaining %>%
    filter(stimulus_type == "prom_cat") %>%
    mutate(trial_type = "prom", block_number = 0)
  
  stim_remaining <- stim_remaining %>%
    slice(which(! stim_remaining$image %in% stim_cat$image))
  
  stim_prm <- stim_remaining %>%
    filter(stimulus_type == "prom_spec") %>%
    mutate(trial_type = "prom",
           block_number = rep((1+n_cond/4):(n_cond-(n_cond/4)),
                              each = n_pm/(n_cond/2)))
  
  stim_remaining <- stim_remaining %>%
    slice(which(! stim_remaining$image %in% stim_prm$image))
  
  stim_ong <- stim_remaining %>%
    filter(stimulus_type == "ongoing") %>%
    slice_head(n = n_stimuli) %>%
    mutate(trial_type = "ongoing",
           block_number = rep(1:n_cond, each = n_stimuli/n_cond))
  
  stim_remaining <- stim_remaining %>%
    slice(which(! stim_remaining$image %in% stim_ong$image))
  
  max_practice_stimuli <- n_practice_trials * max(load)
  stim_pra <- stim_remaining %>%
    slice_head(n = max_practice_stimuli) %>%
    mutate(trial_type = "practice", block_number = 0)
  
  stim_remaining <- stim_remaining %>%
    slice(which(! stim_remaining$image %in% stim_pra$image))
  
  max_prm_trial_filler <- nrow(stim_prm) * (max(load) - 1)
  stim_prm_filler <- stim_remaining %>%
    slice_head(n = max_prm_trial_filler) %>%
    mutate(trial_type = "prom") #, stimulus_seq = rep(1:max_prm_trial_filler))
  
  stim_remaining <- stim_remaining %>%
    slice(which(! stim_remaining$image %in% stim_prm_filler$image))
  
  
  # ========================================= add dynamic parts to image lists ===
  
  n_practice_stimuli <- n_practice_trials * current_load
  temp_pra <- stim_pra %>%
    slice_head(n = n_practice_stimuli) %>%
    mutate(image_v1 = sample(image, length(image)),
           image_v2 = sample(image, length(image))) %>%
    mutate(trial_seq = rep(1:current_load, n_practice_stimuli/current_load)) %>%
    mutate(trial_number = rep(1:n_practice_trials, each = current_load)) 
  
  temp_ong <- stim_ong %>%
    mutate(image_v1 = sample(image, length(image)),
           image_v2 = sample(image, length(image))) %>%
    mutate(trial_seq = rep(1:current_load, nrow(stim_ong)/current_load)) %>%
    mutate(trial_number = rep(1:(nrow(stim_ong)/current_load), each = current_load)) 
  
  prm_positions <- temp_ong %>%
    group_by(block_number) %>%
    summarize(trial_number = max(trial_number)-0.5) %>%
    filter(block_number %in% stim_prm$block_number)
  
  temp_prm <- stim_prm %>%
    mutate(image_v1 = sample(image, length(image)),
           image_v2 = sample(image, length(image))) %>%
    mutate(trial_seq = median(1:current_load)-0.5) %>%
    mutate(trial_number = prm_positions$trial_number)
  
  n_filler_stimuli <- n_pm * (current_load-1)
  
  if(n_filler_stimuli > 0) {
    temp_prm_filler <- stim_prm_filler %>%
      slice_head(n = n_filler_stimuli) %>%
      mutate(image_v1 = sample(image, length(image)),
             image_v2 = sample(image, length(image))) %>%
      mutate(block_number = rep((1+n_cond/4):(n_cond-(n_cond/4)),
                                each = n_pm/(n_cond/2)*(current_load-1))) %>%
      mutate(trial_seq = rep(1:(current_load-1), n_filler_stimuli/(current_load-1))) %>%
      mutate(trial_number = rep(prm_positions$trial_number, each = current_load-1))
    
    temp_all <- bind_rows(temp_pra, temp_ong, temp_prm, temp_prm_filler) %>%
      arrange(block_number, trial_number, trial_seq)
    
  } else {
    temp_all <- bind_rows(temp_pra, temp_ong, temp_prm) %>%
      arrange(block_number, trial_number, trial_seq)
  }
  
  temp_all <- temp_all %>%
    mutate(stimulus_seq = 1:nrow(temp_all),
           trial_seq = rep(1:current_load, nrow(temp_all)/current_load),
           trial_number = rep(1:(nrow(temp_all)/current_load), each = current_load))
  
  
  # ======================================================= create color lists ===
  
  n_cols <- n_colors(load)
  
  for (i in 1:2) {
    
    hues_all <- temp_all %>%
      pivot_wider(id_cols = trial_number, names_from = trial_seq, values_from = hue) %>%
      rowwise() %>%
      mutate(hue = random_colors(n_cols, min_distance)) %>%
      separate(hue, paste("hue", 0:n_cols, sep = "_"), extra = "drop", sep = "\\D+") %>%
      discard(~all(is.na(.) | . == "")) %>%
      #select(starts_with("hue")) %>%
      type_convert()
    
    names <- c(paste("hue", 0:(n_cols-1), sep = "_"), "trial_number")
    
    hues_all <- hues_all %>%
      relocate(sample(2:ncol(.))) %>%
      set_names(., names) %>%
      add_count(hue_0) %>%
      arrange(-n, -hue_0)
    
    hues_remaining <- hues_all
    
    hues_prm <- hues_all %>%
      head(n = nrow(stim_prm))
    
    if(nrow(hues_prm) > hues_all$n[1]) {
      stop("Error: NOT ENOUGH identical colors for the number of prospectice memory cues")
    }
    
    hues_remaining <- hues_all %>%
      slice(which(! hues_remaining$trial_number %in% hues_prm$trial_number))
    
    hues_prm <- hues_prm %>%
      select(-n) %>%
      pivot_longer(!hue_0 & !trial_number, names_to = "trial_seq", values_to = "hue") %>%
      mutate(abs_color_dev = abs(color_dev_deg(hue_0, hue))) %>%
      #separate(trial_seq, c("drop", "trial_seq")) %>%
      #select(-drop) %>%
      type_convert()
    
    hues_filler <- hues_prm
    
    hues_prm <- hues_prm %>%
      group_by(trial_number) %>%
      mutate(max_color_dev = max(abs_color_dev)) %>%
      filter(abs_color_dev == max_color_dev) %>%
      rename(hue_prom = hue_0, trial_number_hues = trial_number) %>%
      select(hue, hue_prom, abs_color_dev) %>%
      mutate(trial_type = "prom", stimulus_type = "prom_spec") %>%
      bind_cols(prm_positions) %>%
      add_column(trial_seq = median(1:current_load)-0.5) %>%
      ungroup() %>%
      select(trial_seq, trial_number, trial_type, stimulus_type, hue, hue_prom,
             abs_color_dev)
    
    hues_filler <- hues_filler %>%
      slice(which(! hues_filler$hue %in% hues_prm$hue)) %>%
      select(trial_number, trial_seq, hue) %>%
      group_by(trial_number) %>%
      mutate(hue = sample(hue, replace = FALSE)) %>%
      pivot_wider(names_from = trial_seq, values_from = hue) %>%
      rename(trial_number_hues = trial_number) %>%
      mutate(trial_type = "prom", stimulus_type = "ongoing") %>%
      bind_cols(prm_positions) %>%
      select(-trial_number_hues) %>%
      pivot_longer(!trial_type & !stimulus_type & !block_number & !trial_number &
                     !trial_number_hues, names_to = "trial_seq", values_to = "hue",
                   names_prefix = "hue_") %>%
      type_convert() %>%
      ungroup()
    
    hues_filler <- hues_filler %>%
      mutate(trial_seq = rep(1:(max(load)-1), nrow(hues_filler)/(max(load)-1) )) %>%
      filter(trial_seq < current_load) %>%
      select(trial_seq, trial_number, trial_type, stimulus_type, hue)
    
    hues_remaining <- hues_remaining %>%
      arrange(trial_number) %>%
      select(-hue_0, -n) %>%
      pivot_longer(!trial_number, names_to = "trial_seq", values_to = "hue",
                   names_prefix = "hue_") %>%
      type_convert() %>%
      group_by(trial_number) %>%
      mutate(hue = sample(hue, replace = FALSE)) %>%
      add_column(trial_type = "ongoing", stimulus_type = "ongoing") %>%
      filter(trial_seq <= current_load) %>%
      ungroup()
    
    hues_remaining <- hues_remaining %>%
      mutate(trial_number = rep(1:(nrow(hues_remaining)/current_load), each = current_load))
    
    hues_pra <- hues_remaining %>%
      slice_tail(n = n_practice_stimuli) %>%
      mutate(trial_type = "practice",
             trial_number = rep(1:(n_practice_stimuli/current_load), each = current_load))
    
    hues_remaining <- hues_remaining %>%
      slice(which(! hues_remaining$trial_number %in% hues_pra$trial_number))
    
    hues_remaining <- hues_remaining %>%
      mutate(trial_number = rep(1:(nrow(hues_remaining)/current_load), each = current_load))
    
    temp_all_hues <- hues_prm %>%
      select(trial_seq, trial_number, trial_type, stimulus_type, hue, hue_prom) %>%
      bind_rows(hues_filler, hues_remaining) %>%
      arrange(trial_number, trial_seq)
    
    temp_all_hues <- bind_rows(hues_pra, temp_all_hues)
    
    temp_all_hues <- temp_all_hues %>%
      mutate(stimulus_seq = 1:nrow(temp_all_hues),
             trial_seq = rep(1:current_load, nrow(temp_all_hues)/current_load),
             trial_number = rep(1:(nrow(temp_all_hues)/current_load), each = current_load))
    
    temp_all_hues$hue      <- temp_all_hues$hue -179
    temp_all_hues$hue_prom <- temp_all_hues$hue_prom -179
    
    names(temp_all_hues)[names(temp_all_hues) == "hue"] <- paste0("hue_v", i)
    names(temp_all_hues)[names(temp_all_hues) == "hue_prom"] <- paste0("hue_prom_v", i)
    
    if (exists("tmp")) {
      temp_all_hues <- full_join(tmp, temp_all_hues,
                                 by = c("trial_number", "trial_seq", "trial_type",
                                        "stimulus_type", "stimulus_seq"))
    }
    
    tmp <- temp_all_hues
    #assign(paste0("hues_prm_v", i), hues_prm)
    
  }
  
  stim_list <- full_join(temp_all, temp_all_hues,
                     by = c("trial_seq", "trial_number", "trial_type",
                            "stimulus_type", "stimulus_seq")) %>%
    select(-image, -hue)
 
  pm_cat_file_num <- parse_number(pm_cat)
  
  filename <- paste0(dir_output, "/stimulus_list_for_reference_image_",
                     pm_cat_file_num, "_load_", current_load, ".csv")
  
  write_delim(stim_list, filename, delim = ',')
  
  rm(hues_all, hues_filler, hues_pra, hues_prm, hues_remaining,
     prm_positions, stim_all, stim_cat, stim_ong, stim_pra, stim_prm,
     stim_prm_filler, stim_remaining, temp_all, temp_all_hues, temp_ong,
     temp_pra, temp_prm, temp_prm_filler, tmp)
  
  }


# ====================================================================== END ===

