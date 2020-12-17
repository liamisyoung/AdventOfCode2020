library(tidyverse)
library(future.apply)

# T1 and T2 are almost the same just with another coordinate. Refactored T1 for a w coordinate of 0.

# Create parallel session
plan(multisession)

# Read data and clean up
init <- read_lines("Data/Day17.txt") %>%
  sapply(., function(line)
    str_split(line, "")) %>%
  unlist()

# Create expanded dataframe of inactive cubes
side <- sqrt(length(init)) + 6 * 2
cubes <- expand.grid(seq(1:side), seq(1:side), seq(-6, 6), 0) %>%
  as_tibble()

# Create expanded initial data
centre_seq <- seq(from = 7, to = side[length(side)] - 6, by = 1)
centre_inds <- expand.grid(centre_seq, centre_seq, 0, 0) %>%
  as_tibble() %>%
  arrange(desc(Var2)) %>%
  mutate(val = init)

# Join on initial values
data_t1 <-
  left_join(cubes, centre_inds, by = c("Var1", "Var2", "Var3", "Var4")) %>%
  replace_na(list(val = ".")) %>%
  rename(x = Var1, y = Var2, z = Var3, w = Var4)

# Task 1 ------------------------------------------------------------------

# Function to return if a row is an active cube or is a neighbour of one
get_neighbours <- function(data) {
  active <- data %>% filter(val == "#")
  
  neighbours <- vector("list", length = nrow(active))
  for (i in 1:nrow(active)) {
    neighbours[[i]] <- data %>%
      filter(x %in% seq(active$x[i] - 1, active$x[i] + 1)) %>%
      filter(y %in% seq(active$y[i] - 1, active$y[i] + 1)) %>%
      filter(z %in% seq(active$z[i] - 1, active$z[i] + 1)) %>% 
      filter(w %in% seq(active$w[i] - 1, active$w[i] + 1))
  }
  neighbours <-
    bind_rows(neighbours) %>% 
    mutate(is_neighbour = TRUE) %>% 
    unique()
  
  data <- left_join(data, neighbours, by = c("x", "y", "z", "w", "val")) %>% 
    replace_na(list(is_neighbour = FALSE))
  
  return(data)
}

# Function to get the new state of a current coordinate
get_state <- function(data, x_val, y_val, z_val, w_val) {
  
  # Get current state
  current_state <- data %>%
    filter(x == x_val) %>%
    filter(y == y_val) %>%
    filter(z == z_val) %>%
    filter(w == w_val) %>% 
    pull(val)
  
  # Get the count of neighbour values
  neighbours <- data %>%
    filter(x %in% seq(x_val - 1, x_val + 1)) %>%
    filter(y %in% seq(y_val - 1, y_val + 1)) %>%
    filter(z %in% seq(z_val - 1, z_val + 1)) %>%
    filter(w %in% seq(w_val - 1, w_val + 1)) %>%
    filter(!(x == x_val & y == y_val & z == z_val & w == w_val)) %>%
    group_by(val) %>%
    summarise(count = n())
  
  # Set n_active and n_inactive, accounting for border cases
  n_active <- neighbours %>% filter(val == "#") %>% pull(count)
  n_active <- ifelse(length(n_active) == 0, 0, n_active)
  n_inactive <- neighbours %>% filter(val == ".") %>% pull(count)
  n_inactive <- ifelse(length(n_inactive) == 0, 0, n_inactive)
  
  # Set new state from rules
  ifelse(
    current_state == ".",
    ifelse(n_active == 3, "#", "."),
    ifelse(n_active %in% c(2, 3), "#", ".")
  )
}

# Function to run a sigle iteration
run_iteration <- function(data) {
  
  data <- get_neighbours(data)
  
  print(paste("searching", sum(data$is_neighbour), "neighbours"))
  
  #Pre-allocate for speed
  new_vals <- vector("character", length = nrow(data))  
  
  neighbours <- data %>% filter(is_neighbour == TRUE)
  
  # Call get_neighbours on each row, running in parrallel (about 6 times faster)
  new_vals <- future_apply(neighbours, 1, function(row) {
    get_state(data, as.integer(row[["x"]]), as.integer(row[["y"]]), as.integer(row[["z"]]), as.integer(row[["w"]]))
  })
  
  neighbours <- neighbours %>% mutate(new_val = new_vals) %>% select(-is_neighbour)
  
  data <- left_join(data, neighbours, by = c("x", "y", "z", "w", "val")) %>% 
    replace_na(list(new_val = ".")) %>% 
    select(-is_neighbour, -val) %>% 
    rename(val = new_val)
  
  # # Change value in data to new vals and return
  # data$val <- new_vals
  return(data)
}

# Run all six iterations
for (i in 1:6) {
  data_t1 <- run_iteration(data_t1)
}

# Get answer
data_t1 %>% group_by(val) %>% summarise(count = n())

# Task 2 ------------------------------------------------------------------

# Pretty much all the same as above but add a w coordinate, analagous to another z really

# Update cubes with new w coordinate
cubes <- expand.grid(seq(1:side), seq(1:side), seq(-6, 6), seq(-6, 6)) %>%
  as_tibble()

# Add baseline w coordinate
centre_inds <- centre_inds %>% mutate(Var4 = 0)

# Create data
data_t2 <-
  left_join(cubes, centre_inds, by = c("Var1", "Var2", "Var3", "Var4")) %>%
  replace_na(list(val = ".")) %>%
  rename(x = Var1, y = Var2, z = Var3, w = Var4)

# Run all six iterations
for (i in 1:6) {
  data_t2 <- run_iteration(data_t2)
}

# Get answer
data_t2 %>% group_by(val) %>% summarise(count = n())