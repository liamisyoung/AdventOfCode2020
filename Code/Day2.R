# Read data and manipulate
data <- readr::read_delim("~/AdventOfCode2020/Data/Day2.txt", delim = " ", col_names = c("limits", "character", "password")) %>%
  tidyr::separate(limits, c("lower", "upper"), sep = "-") %>%
  dplyr::mutate(character = str_remove(character, ":"))

# Task 1 ------------------------------------------------------------------

# Write function to apply over rows
valid_password_t1 <- function(lower, upper, character, password) {
  str_count(password, character) %in% seq(from = lower, to = upper, by = 1)
}

# Apply function over each row
data <- data %>%
  mutate(
    valid_rows_t1 = apply(., 1, function(row) valid_password_t1(row['lower'], row['upper'], row['character'], row['password']))
  )

# Get sum of valid passwords
t1_ans <- sum(data$valid_rows_t1)

# Task 2 ------------------------------------------------------------------

# Again, write function to apply over rows
valid_password_t2 <- function(first, second, character, password) {
  # Get all locations of character
  ind <- str_locate_all(password, character)
  # Check for first and second positions in returns indices
  first_true <- first %in% ind[[1]]
  second_true <- second %in% ind[[1]]
  # Return true if logicals sum to 1 (i.e. only 1 present)
  if (first_true + second_true == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Apply function over each row
data <- data %>%
  mutate(
    valid_rows_t2 = apply(., 1, function(row) valid_password_t2(row['lower'], row['upper'], row['character'], row['password']))
  )

# Get sum of valid passwords
t2_ans <- sum(data$valid_rows_t2)
