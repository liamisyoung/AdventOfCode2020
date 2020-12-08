library(tidyverse)

# Read data and clean up
data <- read_lines("Data/Day8.txt") %>%
  as_tibble() %>%
  separate(value,
           into = c("instr", "val"),
           sep = " ") %>% 
  mutate(val = as.integer(val))


# Task 1 ------------------------------------------------------------------

# Functionised after seeing task 2
run_loop <- function(data) {
  # Pre-specify values, set exit as false and all others to defaults
  exit <- FALSE
  row <- 1
  total <- 0
  visited <- c() # Array of "visited" rows on our journey through the gameboy
  
  while (exit == FALSE) {
    # Check to see if we've already visited the row, if so exit with convergence -1 i.e. loop
    if (row %in% visited) {
      exit = TRUE
      convergence = -1
      # Otherwise check to see if we've reached the end! Wooooo
    } else if (row == nrow(data) + 1) {
      exit = TRUE
      convergence = 1
      # If not, plow on.
    } else {
      
      # Get instruction and value for the row
      step_instr <- data[row, ]$instr
      step_val <- data[row, ]$val
      
      # Add the row to the visited list, we don't like going to see the same one twice.
      visited <- c(visited, row)
      
      # Do the actual instruction for the step
      if (step_instr == "acc") {
        total <- total + step_val
        row <- row + 1
      } else if (step_instr == "nop") {
        row <- row + 1
      } else if (step_instr == "jmp") {
        row <- row + step_val
      }
    }
  }
  
  # Once the while loop has ended, return the accumulator value and how the loop finished.
  # I.e. convergence = -1, the loop has started again, convergence = 1, we've fixed it!
  return(c(total, convergence))
}

# Task 2 ------------------------------------------------------------------

# First idea, brute force all nop to jmp, why not?

# Get all indices of nop instructions
nop_ind <- which(data$instr == "nop")

# One by one change them and run the loop, checking for convergence == 1
for (i in 1:length(nop_ind)) {
  data_temp <- data
  data_temp[nop_ind[i], ]$instr <- "jmp"
  
  val <- run_loop(data_temp)
  
  if (val[2] == 1) {
    print(val)
  }
}

# Now do all the jmp

# Get all indices of jmp instructions
jmp_ind <- which(data$instr == "jmp")

# One by one change them and run the loop, checking for convergence == 1
for (i in 1:length(jmp_ind)) {
  data_temp <- data
  data_temp[jmp_ind[i],]$instr <- "nop"
  
  val <- run_loop(data_temp)
  
  if(val[2] == 1) {
    print(val)
  }
}
