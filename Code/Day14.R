library(tidyverse)
library(binaryLogic)

# Read data and clean up
data <- read_lines("Data/Day14.txt") %>% 
  enframe(name = NULL) %>% 
  separate(value,
           into = c("instr", "val"),
           sep = " = ") %>% 
  separate(instr,
           into = c("instr", "mem_pos"),
           sep = "\\[") %>% 
  mutate(mem_pos = as.integer(str_remove_all(mem_pos, "\\]")))

# Task 1 ------------------------------------------------------------------

# To binary helper function
# The functions I used didn't handle the integer size i.e. 36 bit so needed
# to create my own function. Worked a charm.
from_binary <- function(bin) {
  sum <- 0
  for (i in 1:length(bin)) {
    if (bin[i]) {
      sum = sum + 2^(36 - i) 
    }
  }
  return(sum)
}

# Pre-allocate vector
memory <- vector(mode = "integer", length = max(data$mem_pos, na.rm = TRUE))

# Go through all rows
for (i in 1:nrow(data)) {
  # Override mask
  if (data$instr[i] == "mask") {
    current_mask <- data$val[i]
  # For all memory, apply mask and then save to appropriate memory
  } else {
    bin <- fillUpToBit(as.binary(data$val[i]), 36, value = FALSE)
    bin[str_locate_all(current_mask, "1")[[1]][, 1]] <- TRUE
    bin[str_locate_all(current_mask, "0")[[1]][, 1]] <- FALSE
    memory[data$mem_pos[i]] <- from_binary(bin)
  }
}

t1 <- sum(memory)

# Task 2 ------------------------------------------------------------------

# Create a generate_locations function. Takes in the integer value of the location
# converts it to binary, finds all combinations based on the floating values in the mask
# and returns all integer locations.
generate_locations <- function(address, mask) {
  address_bin <- fillUpToBit(as.binary(address), 36, value = FALSE)
  
  ones <- str_locate_all(mask, "1")[[1]][,1]
  float <- str_locate_all(mask, "X")[[1]][,1]
  address_bin[ones] <- TRUE
  locations <- vector("list", length = 2 ^ length(float))
  
  ind <-
    expand.grid(replicate(length(float), c(0, 1), simplify = FALSE)) %>% as.matrix()
  
  for (i in 1:nrow(ind)) {
    temp_address <- address_bin
    temp_address[float] <- ind[i,]
    locations[[i]] <- from_binary(temp_address)
  }
  
  return(unlist(locations))
}

# I did a pre-run of the generate locations to get a pre-allocation length...
memory_t2 <- tibble(loc = numeric(), val = numeric())

# Go through each row
for (i in 1:nrow(data)) {
  # Override mask
  if (data$instr[i] == "mask") {
    current_mask <- data$val[i]
  # Add rows to memory_t2 dataframe with the values and locations of new memory
  } else {
    locations <- generate_locations(data$mem_pos[i], current_mask)
    vals <- rep(as.integer(data$val[i]), length(locations))
    memory_t2 <-
      bind_rows(memory_t2, tibble(loc = locations, val = vals))
  }
}

# Add a sequence column to the new memory
memory_t2 <- memory_t2 %>% 
  add_column(id = seq(1:nrow(memory_t2)))

# Aggregate new memory by the sequence, keeping the highest value only
t2 <- sum(memory_t2[aggregate(memory_t2$id, by = list(memory_t2$loc), max)$x,]$val)
