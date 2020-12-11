library(tidyverse)

# Read data and clean up
data <- read_lines("Data/Day11Test.txt") %>%
  sapply(., function(line) str_split(line, ""))

# Convert to matrix
data <- matrix(unlist(data), ncol = length(data[[1]]), byrow = TRUE)

# These work but take AAAAAGES to run
# I could've combined the iterate and run in to a single function and passed in
# the appropriate get neighbour function as an arg, but honestly it worked and I
# can't be bothered now.

# Task 1 ------------------------------------------------------------------

# Get values of neighbours
get_neighbours <- function(data, row, col) {
  # Create long dataframe of neighbours one in each direction
  neighbours <- expand.grid(seat_row = seq((row - 1),(row + 1)), seat_col = seq((col - 1),(col + 1))) %>% 
    # Filter out our current seat
    filter((seat_row != row) | (seat_col != col)) %>% 
    # Filter out seats outside the boundaries
    filter((seat_row != 0) & (seat_col != 0)) %>%
    filter((seat_row <= nrow(data)) & (seat_col <= ncol(data))) %>% 
    # Tibble
    as_tibble() %>%
    # Get the value for each seat indice
    rowwise() %>% 
    mutate(seat_val = data[seat_row, seat_col]) %>% 
    ungroup()
}

# Run a single "seating flip iteration"
iterate <- function(data) {
  # Copy data to new data
  new_data <- data
  # Loop across all seats
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      # Current seat value
      state <- data[i, j]
      
      # Get count of seat status for neighbours
      neighbours <- get_neighbours(data, i, j) %>% 
        group_by(seat_val) %>% 
        summarise(count = n())
      
      # Get out num occupied
      occupied <- (neighbours %>% filter(seat_val == "#"))$count
      if (length(occupied) == 0) {
        occupied <- 0
      }
      
      # If needed, flip the seat state
      if (state == "L" & occupied == 0) {
        new_data[i, j] <- "#"
      } else if (state == "#" & occupied >= 4) {
        new_data[i, j] <- "L"
      }
    }
  }
  
  return(new_data)
}

# Keep running iterations until change is 0
run_t1 <- function(data) {
  changes <- TRUE
  iter <- 0
  
  # Keep iterating and checking if seat layout has changed
  while (changes == TRUE) {
    new_data <- iterate(data)
    if (sum(data != new_data) == 0) {
      changes = FALSE
    }
    data <- new_data
    iter <- iter + 1
    print(paste("iter", iter))
  }
  return(sum(data == "#"))
}

# Run the seating process
run_t1(data)

# Task 2 ------------------------------------------------------------------

# Get value of the nearest seat in any direction
get_nearest_in_dir <- function(data, row, col, up, right) {
  found <- FALSE
  # Keep going one further out in the direction until a seat is found
  while (found == FALSE) {
    row <- row - up
    col <- col + right
    # If direction goes out of bounds, return "."
    if (!(row %in% seq(1:nrow(data))) | !(col %in% seq(1:ncol(data)))) {
      return(".")
    } else if (data[row, col] == "L") {
      return("L")
    } else if (data[row, col] == "#") {
      return("#")
    }
  }
}

# Get values of all nearest in all directions
get_dir_neighbours <- function(data, row, col) {
  dirs <-
    # Specify directions
    tibble(up = c(1, 1, 1, -1, -1, -1, 0, 0),
           right = c(-1, 0, 1, -1, 0, 1, 1, -1)) %>% 
    rowwise() %>% 
    # Get value for each direction
    mutate(val = get_nearest_in_dir(data, row, col, up, right)) %>% 
    ungroup()
  
  return(dirs)
}

# Run a single "seating flip iteration"
iterate_t2 <- function(data) {
  # Copy data to new data
  new_data <- data
  # Loop across all seats
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      # Current seat value
      state <- data[i, j]
      
      # Get count of seat status for directional views
      neighbours <- get_dir_neighbours(data, i, j) %>% 
        group_by(val) %>% 
        summarise(count = n())

      # Get out num occupied
      occupied <- (neighbours %>% filter(val == "#"))$count
      if (length(occupied) == 0) {
        occupied <- 0
      }
      
      # If needed, flip the seat state
      if (state == "L" & occupied == 0) {
        new_data[i, j] <- "#"
      } else if (state == "#" & occupied >= 5) {
        new_data[i, j] <- "L"
      }
    }
  }
  
  return(new_data)
}

# Keep running iterations until change is 0
run_t2 <- function(data) {
  changes <- TRUE
  iter <- 0
  
  # Keep iterating and checking if seat layout has changed
  while (changes == TRUE) {
    new_data <- iterate_t2(data)
    if (sum(data != new_data) == 0) {
      changes = FALSE
    }
    data <- new_data
    iter <- iter + 1
    print(paste("iter", iter))
  }
  return(sum(data == "#"))
}

# Run the seating process
run_t2(data)