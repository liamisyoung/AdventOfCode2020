library(tidyverse)

# Read data
data <- read_lines("~/AdventOfCode2020/Data/Day4.txt") %>%
  paste(sep = "", collapse = " ") %>%
  str_split("  ") %>%
  unlist()

# Task 1 ------------------------------------------------------------------

# Required fields for T1
req_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
opt_fields <- c("cid")

# Function to validate passport
validate_passport_t1 <- function(passport, req_fields, opt_fields) {
  fields_present <-
    sapply(req_fields, function(field)
      str_detect(passport, field))

  if (sum(fields_present) == 7) {
    valid = TRUE
  } else {
    valid = FALSE
  }
  return(valid)
}

# Apply check over all passports
valid_passports_t1 <- sapply(data, function(passport) validate_passport_t1(passport, req_fields, opt_fields))

# Get sum
t1_ans <- sum(valid_passports_t1)

# Task 2 ------------------------------------------------------------------

# Only use passports already deemed valid
data_t2 <- data[valid_passports_t1]

# Function to validate values
validate_values <- function(passport) {
  # Split in to tibble
  passport_df <- str_split(passport, " ")[[1]] %>%
    as_tibble() %>%
    separate(value, c("col", "val"), ":") %>%
    pivot_wider(names_from = col, values_from = val)

  # Valid bearth year
  valid_byr <- (passport_df$byr >= 1920) & (passport_df$byr <= 2002)
  # Valid issue year
  valid_iyr <- (passport_df$iyr >= 2010) & (passport_df$iyr <= 2020)
  # Valid expiry year
  valid_eyr <- (passport_df$eyr >= 2020) & (passport_df$eyr <= 2030)
  # Valid height
  valid_hgt <- if (str_sub(passport_df$hgt, start = -2) == "cm") {
    (str_replace(passport_df$hgt, "cm", "") >= 150) & (str_replace(passport_df$hgt, "cm", "") <= 193)
  } else if (str_sub(passport_df$hgt, start = -2) == "in") {
    (str_replace(passport_df$hgt, "in", "") >= 59) & (str_replace(passport_df$hgt, "in", "") <= 76)
  } else {
    FALSE
  }
  # Valid hair colour
  valid_hcl <- str_detect(passport_df$hcl, "[#][a-f0-9]{6}")
  # Valid eye colour
  valid_ecl <- (passport_df$ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) & (nchar(passport_df$ecl) == 3)
  # Valid passport ID
  valid_pid <- str_detect(passport_df$pid, "[0-9]{9}") & nchar(passport_df$pid) == 9

  # Return validity
  if (all(valid_byr, valid_iyr, valid_eyr, valid_hgt, valid_hcl, valid_ecl, valid_pid)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Apply valid values function over all t1 valid passports
valid_passports_t2 <- sapply(data_t2, function(passport) validate_values(passport))

# Get sum
t2_ans <- sum(valid_passports_t2)

