# Read data
data <-
  readr::read_csv("~/AdventOfCode2020/Data/Day1.txt",  col_names = FALSE)
nums <- data$X1

# Task 1 ------------------------------------------------------------------

# Initial idea, complete enumeration (loop through all conditions)
for (i in 1:length(nums)) {
  for (j in i + 1:length(nums)) {
    # Check if adds to 2020
    if (nums[i] + nums[j] == 2020) {
      task1ans <- c(nums[i], nums[j])
    }
  }
}

# Faster approach, only one loop.
for (i in 1:length(nums)) {
  # For each number, if 2020 - number is in the list of numbers, then done.
  if ((2020 - nums[i]) %in% nums) {
    task1ans_v2 <- c(nums[i], 2020 - nums[i])
  }
}


# Task 2 ------------------------------------------------------------------

# Slow approach again, with third loop. VERY SLOW ~ 6 seconds
for (i in 1:length(nums)) {
  for (j in i + 1:length(nums)) {
    for (k in 1:length(nums)) {
      if (nums[i] + nums[j] + nums[k] == 2020) {
        task2ans <- c(nums[i], nums[j], nums[k])
      }
    }
  }
}

# Faster approach again, still requires two loops.
for (i in 1:length(nums)) {
  for (j in 1:length(nums)) {
    if (i != j) {
      if ((2020 - nums[i] - nums[j]) %in% nums) {
        task2ans_v2 <- c(nums[i], nums[j], 2020 - nums[i] - nums[j])
      }
    }
  }
}
