library(tidyverse)

# Task 1 ------------------------------------------------------------------

# Read data and clean up
data <-
  read_lines("Data/Day10.txt") %>%
  as.integer() %>%
  enframe(name = NULL) %>%
  arrange(value)

# Add max value on end and calculate differences
data <- data %>% 
  bind_rows(tibble(value = max(data$value) + 3)) %>% 
  mutate(diff = diff(c(0, value)))

# Summarise 1's and 3's and get answer
summarised <- data %>% 
  select(diff) %>% 
  group_by(diff) %>% 
  summarise(count = n())

# Answer
t1_ans <- prod(summarised$count)

# Task 2 ------------------------------------------------------------------

# Using indices all shifted up 1 for task 2 to create a first value of 1
# i.e. there is 1 combination to have the outlet value of 0.

# Create vector of "number of ways to n" and set first value to 1 i.e. outlet
n <- vector(length = max(data$value) + 1)
n[1] <- 1

# Add the 0'th value to the values vector and shift all 1 up as outlined above
vals <- c(1, data$value + 1)

# Number of ways to n is the sum of the ways to n-1, n-2 and n-3, as you can always reach the value
# with a difference less than 3. Can work up from the first one building up a tree. Important 
# to include the 0's for joltage values not included in the adapter list.
for (i in 2:length(vals)) {
  val <- vals[i]
  
  # Find which values are less then 3 before the current value
  prev_vals <-
    vals[vals %in% seq(from = val - 3,
                       to = val - 1,
                       by = 1)]
  
  # Add the ways to the previous 3 values and store
  n[val] <- sum(n[prev_vals])
}

t2_ans <- n[length(n)]

