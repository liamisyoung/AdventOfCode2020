library(tidyverse)

# Read data
data <- read_lines("Data/Day5.txt")

# Task 1 ------------------------------------------------------------------

# Write function to get row and column
get_seat <- function(pass) {
  
  # Replace letters with binary
  pass <- pass %>%
    str_replace_all("F", "0") %>%
    str_replace_all("B", "1") %>%
    str_replace_all("L", "0") %>%
    str_replace_all("R", "1")

  # Convert binary strings to int
  row <- strtoi(substring(pass, 1, 7), base = 2)
  col <- strtoi(substring(pass, 8, 10), base = 2)
  
  return(row * 8 + col)
}

# Apply function across all boarding passes and get max
seat_ids <- sapply(data, get_seat)
t1_ans <- max(seat_ids)


# Task 2 ------------------------------------------------------------------

# Create sequence of all seat ids from front to back
# Note - we know our seat is in the middle somewhere
all_seats <- seq(from = min(seat_ids), to = max(seat_ids), by = 1)

# Get true/false whether seat id from seq is in seat ids from above
seats_filled <- all_seats %in% seat_ids

# Find the one missing :)
t2_ans <- all_seats[!seats_filled]
