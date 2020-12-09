library(tidyverse)
library(bit64)

# Read data and clean up
data <- read_lines("Data/Day9.txt") %>% as.integer64()

# Task 1 ------------------------------------------------------------------

# Again write as function to use on test data first, setting  n = 5 before tackling the main stuff
get_error <- function(data, n) {
  
  # Pre populate list
  sums <- vector("list", length = length(data) - (n + 1))
  
  # Do all values past the preamble
  for (i in (n + 1):length(data)) {
    # set initial values
    val <- data[i]
    match_found <- vector(length = n)
    ind <- 1
    # Loop across previous n values
    for (j in (i - n):(i - 1)) {
      new_val <- val - data[j]
      # If target value - value is in previous n, we have a winner
      if ((new_val %in% data[(i - n):(i - 1)]) &
          (new_val != data[j])) {
        match_found[ind] <- TRUE
      } else {
        match_found[ind] <- FALSE
      }
      ind <- ind + 1
    }
    # Populate the list with the correct match we found
    sums[[i - n]] <- data[(i - n):(i - 1)][match_found]
  }
  # Return which one has no match, i.e. length == 0
  return(data[which(sapply(sums, length) == 0) + n])
}

# Get the answer
t1_ans <- get_error(data, 25)

# Task 2 ------------------------------------------------------------------

# Functionise once more to test on n = 5
get_error_fix <- function(data, error_val) {
  # Start at the beginning, we haven't found a match yet
  found <- FALSE
  i <- 1
  
  # Nested while loop. First time for everything. Exit first loop when we get a match
  while (found == 0) {
    # Start adding numbers together from i, adding to val
    j <- i
    val <- 0
    
    # Keep adding numbers if the running val is less than the target error
    while (val < error_val) {
      val <- val + data[j]
      j <- j + 1
    }
    
    # Once while loop has ended, if the value is the error value, WE WIN
    if (val == error_val) {
      found <- TRUE
    } else {
      i <- i + 1
    }
  }
  
  # Get the contiguous values and get the weakness
  contig <- data[i:(j - 1)]
  return(min(contig) + max(contig))
}

t2_ans <- get_error_fix(data, t1_ans) 
