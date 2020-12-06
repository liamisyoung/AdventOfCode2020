library(tidyverse)

# Task 1 ------------------------------------------------------------------

# Read data
data <- read_lines("Data/Day6.txt")  %>% 
  paste(sep = "", collapse = " ") %>% 
  str_split("  ") %>% 
  unlist()

# Apply across all groups, detect each character, sum across. 
t1_ans <-
  sum(sapply(data, function(group)
    sum(str_detect(group, letters))))

# Task 2 ------------------------------------------------------------------

# Function to get where all were answered
get_all_answered <- function(group) {
  # Split string in to individuals
  individuals <- str_split(group, " ") %>%
    unlist()
  
  # Get which questions an individual answered, stored in group matrix
  answered <-  sapply(individuals, function(individual)
    str_detect(individual, letters))
  
  # Detect where all were answered and get sum
  return(sum(apply(answered, 1, all)))
}

# Apply function across all groups and get sum
t2_ans <- sum(sapply(data, get_all_answered))