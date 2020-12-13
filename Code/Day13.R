library(tidyverse)
library(Rfast2)

# Read data and clean up
text <- read_lines("Data/Day13.txt")

test1 <- c(7,13,"x","x",59,"x",31,19)
test2 <- c(17,"x",13,19)
test3 <- c(67,7,59,61)
test4 <- c(67,"x",7,59,61)
test5 <- c(67,7,"x",59,61)
test6 <- c(1789,37,47,1889)

buffer <- 100

bus <- text[2] %>% 
  str_remove_all("x") %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()

bus_vals <- bus[!is.na(bus)]
bus_names <- paste0("bus", bus_vals)

bus_names_df <-
  bus_names %>% purrr::map_dfc(setNames, object = list(rep(NA, as.integer(text[1]) + buffer)))

data <- tibble(
  time = seq(from = 1, to = as.integer(text[1]) + buffer, by = 1)
) %>% 
  bind_cols(bus_names_df)

# Task 1 ------------------------------------------------------------------

# Get sequence of all busses
bus_seq <- sapply(bus_vals, function(bus) {
  seq(from = 0, to = nrow(data), by = bus)
})

# Mark as departing for each appropriate indice
for (i in 1:length(bus_seq)) {
  data[[bus_names[i]]][bus_seq[[i]]] <- "D"
}

# Manually look for next bus and do calculation
data %>% 
  filter(time >= as.integer(text[1]))

t1 <- 601 * (1002468 - 1002461)

# Task 2 ------------------------------------------------------------------

# Initial idea was to shift all busses by the minutes after bus 1, but the vector
# memory got wrecked and wouldn't go large enough... Worked on the test cases real
# nice though.

# Then tried creating a remainder index for all bus id's and building a sequence of 
# the first bus times, then checking all subsequence busses leave at time + remainder.
# For loops are painfully slow though...

# Next idea was to iteratively build an intersection vector, adding busses one by one.

# Finally solved with a hybrid of idea 1

# Get the minutes each bus should be shifted, then start with the biggest
bus_rem_df <-
  tibble(bus = bus, rem = seq(
    from = 0,
    to = length(bus) - 1,
    by = 1
  )) %>%
  filter(!is.na(bus)) %>%
  arrange(desc(bus))

# Create an initial sequence of the largest bus id, shifted by the remaining value
start <- 0
end <- 9999999
current_seq <- seq(from = start, to = end, by = bus_rem_df$bus[1]) - bus_rem_df$rem[1]

# For all other bus id's, filter the current sequence based on the correct remainder
# then generate a new sequence from the difference. After all have been generated/filtered
# the first value in the end sequence will be the first timestamp appropriate
for (i in 2:nrow(bus_rem_df)) {
  new_seq <- current_seq[(current_seq + bus_rem_df$rem[i]) %% bus_rem_df$bus[i] == 0]
  diff <- diff(new_seq)[1]
  start <- new_seq[1]
  end <- start + diff * 1000
  rm(current_seq)
  current_seq <- seq(from = start, to = end, by = diff)
}

# Get the answer
t2_ans <- start
