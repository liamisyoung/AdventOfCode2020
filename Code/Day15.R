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
  
  # Create lookup table. Position i + 1 will store the most recent "turn" number i was spoken
  # The +1 is required as R indexes from 1, and we gotta store 0
  recent <- vector("integer", length = n + 1)
  
  # Shorten the data by 1 and set val to the final value in the initial data
  trim <- head(data,-1)
  val <- tail(data, 1)
  
  # Populate the lookup table, but shift up one - gotta remember 0
  # e.g. At the start, position 10 will be 1, saying 9 was said at turn 1
  #                    position 13 will be 2, saying 12 was said at turn 2 etc. etc.
  recent[trim + 1] <- which(trim == trim)
  
  # Start at the last value of the initial data, and go until n - 1
  # and then the final val will be what elf n should say
  for (i in length(data):(n - 1)) {
    # Check to see if val has been said before
    # val is initially set to the last of the initial data, as above
    last <- recent[val + 1]
    # Update the recency vector with the time the number was said
    recent[val + 1] <- i
    # If last is 0 i.e. not said, then say 0, otherwise say the difference in times
    val <- ifelse(last == 0, 0, i - last)
  }
  
  # Return the last value
  return(val)
}

# Run task 1
t1 <- run_game(data, 2020)

# Run task 2
t2 <- run_game(data, 30000000)



