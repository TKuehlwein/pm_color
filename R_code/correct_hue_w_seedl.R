install.packages("viridis")
library(viridis)
viridis_pal(option = "D")(50)  # n = number of colors seeked

set.seed(260423)
runif(n=4, min=-180, max=180) # random number -180 to 180 for pm color


generate_blocks <- function(num_blocks) {
  blocks <- list()
  
  for (i in 1:num_blocks) {
    block <- numeric()
    
    while (length(block) < 5) {
      new_num <- runif(1, -180, 180)  # Generate a random number between -180 and 180
      
      # Ensure new_num differs by at least 40 from all existing numbers in the block
      if (all(abs(new_num - block) >= 40)) {
        block <- c(block, new_num)
      }
    }
    
    blocks[[i]] <- block
  }
  
  return(blocks)
}

# Generate blocks
result <- generate_blocks(10)  # Number of blocks
print(result)

##________________________________________________________________________

## second version, working as intended

##________________________________________________________________________

# clear workspace
rm(list=ls())

# seed l1 v1,3,5: 260423
# seed l1 v2,4,6: 260424
# seed l3 v2,4,6: 260425
# seed l3 v2,4,6: 260426
# seed l5 v1,3,5: 260427
# seed l5 v2,4,6: 260428

set.seed(260428) # seed to recreate pm color
pm_col <- runif(n=1, min=-140, max=140) # random number for pm color

# define function for values
generate_blocks_excluded_range <- function(num_blocks) {
  blocks <- list()
  
  for (i in 1:num_blocks) {
    block <- numeric()
    
    while (length(block) < 5) {
      new_num <- runif(1, -180, 180)  # Generate a random number between -180 and 180
      
      # Ensure new_num differs by at least 40 from all existing numbers in the block
      # and is not in the excluded range
      if (all(abs(new_num - block) >= 40) && !(new_num > (pm_col - 40) & new_num <= (pm_col + 40))) {
        block <- c(block, new_num)
      }
    }
    
    blocks[[i]] <- block
  }
  
  return(blocks)
}

# Generate blocks

result_excluded_range <- generate_blocks_excluded_range(24)  # Number of blocks
print(result_excluded_range)

## __________________________________________________________________________

