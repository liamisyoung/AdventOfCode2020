library(tidyverse)

# Set data
data <- c(9,12,1,4,17,0,18)

# Initially took the value and looked back through the vector, but for task 2 took WAY TOO LONG
# Changed tack, like the other day to flip the vector storage around, storing the most recent time a number was spoken
# rather than the number itself.

# At first tried running it on the current val and looking back, but need to access the previous 2 times a number
# was used, so need to shorten the vector and do on the "next val"

# Function to run game
run_game <- function(data, n) {
  # Create lookup table
  recent <- vector("integer", length = n + 1)
  
  # Shorten the data by 1 and set val to the next one
  trim <- head(data,-1)
  val <- tail(data, 1)
  
  # Populate the lookup table, but shift up one - gotta remember 0
  recent[trim + 1] <- which(trim == trim)
  
  for (i in (length(trim) + 1):(n - 1)) {
    last <- recent[val + 1]
    recent[val + 1] <- i
    val <- ifelse(last == 0, 0, i - last)
  }
  
  return(val)
}

t1 <- run_game(data, 2020)

t2 <- run_game(data, 30000000)



