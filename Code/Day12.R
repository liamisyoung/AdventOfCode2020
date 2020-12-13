library(tidyverse)

# I'll be honest this code isn't that pretty but it works and it was a nice sunny
# saturday so didn't spend time cleaning it up.

# Read data and clean up
data <- read_lines("Data/Day12.txt") %>%
  enframe(name = NULL) %>%
  mutate(dir = substr(value, 1, 1),
         val = parse_number(value)) %>%
  select(-value)

# Task 1 ------------------------------------------------------------------

# Can extract all fixed dirs and sum first
fixed_dirs <- data %>%
  filter(data$dir %in% c("N", "S", "E", "W")) %>%
  group_by(dir) %>%
  summarise(val  = sum(val)) %>%
  pivot_wider(names_from  = dir, values_from = val)

EW_d <- fixed_dirs$E - fixed_dirs$W
NS_d <- fixed_dirs$N - fixed_dirs$S

# Now get rotational dirs
rot_dirs <- data %>%
  filter(data$dir %in% c("L", "R", "F")) %>%
  mutate(uid = 1:nrow(.)) %>%
  select(3, 1:2)

current_bearing <- 90
rot_vals <- c(
  "N" = 0,
  "S" = 0,
  "E" = 0,
  "W" = 0
)
dir_lookup <- c(
  "N" = 0,
  "S" = 180,
  "E" = 90,
  "W" = 270
)

for (i in 1:nrow(rot_dirs)) {
  dir <- rot_dirs$dir[i]
  current_dir <- names(which(dir_lookup == current_bearing))
  if (dir == "F") {
    rot_vals[[current_dir]] <- rot_vals[[current_dir]] + rot_dirs$val[i]
  } else if (dir == "R") {
    current_bearing <- (current_bearing + rot_dirs$val[i]) %% 360
  } else if (dir == "L") {
    current_bearing <-
      (current_bearing + (360 - rot_dirs$val[i])) %% 360
  }
}

EW_r_d <- rot_vals[["E"]] - rot_vals[["W"]]
NS_r_d <- rot_vals[["N"]] - rot_vals[["S"]]

EW <- EW_d + EW_r_d
NS <- NS_d + NS_r_d
t1 <- abs(EW) + abs(NS)

# Task 2 ------------------------------------------------------------------

data_t2 <- data %>%
  add_column(
    w_up = vector("integer", length = nrow(data)),
    w_right = vector("integer", length = nrow(data)),
    up_pos = vector("integer", length = nrow(data)),
    right_pos = vector("integer", length = nrow(data))
  )

# Loop over all instructions filling don
for (i in 1:nrow(data_t2)) {
  prev_up <- ifelse(i == 1, 0, data_t2$up_pos[i - 1])
  prev_right <- ifelse(i == 1, 0, data_t2$right_pos[i - 1])
  prev_w_up <- ifelse(i == 1, 1, data_t2$w_up[i - 1])
  prev_w_right <- ifelse(i == 1, 10, data_t2$w_right[i - 1])
  
  # A bulk load of if else... If you're reading these comments have fun
  if (data_t2$dir[i] == "F") {
    data_t2$w_up[i] <- prev_w_up
    data_t2$w_right[i] <- prev_w_right
    data_t2$up_pos[i] <- prev_up + data_t2$w_up[i] * data_t2$val[i]
    data_t2$right_pos[i] <- prev_right + data_t2$w_right[i] * data_t2$val[i]
  } else if (data_t2$dir[i] == "N") {
    data_t2$w_up[i] <- prev_w_up + data_t2$val[i]
    data_t2$w_right[i] <- prev_w_right
    data_t2$up_pos[i] <- data_t2$up_pos[i - 1]
    data_t2$right_pos[i] <- data_t2$right_pos[i - 1]
  } else if (data_t2$dir[i] == "S") {
    data_t2$w_up[i] <- prev_w_up - data_t2$val[i]
    data_t2$w_right[i] <- prev_w_right
    data_t2$up_pos[i] <- data_t2$up_pos[i - 1]
    data_t2$right_pos[i] <- data_t2$right_pos[i - 1]
  } else if (data_t2$dir[i] == "E") {
    data_t2$w_up[i] <- prev_w_up
    data_t2$w_right[i] <- prev_w_right + data_t2$val[i]
    data_t2$up_pos[i] <- data_t2$up_pos[i - 1]
    data_t2$right_pos[i] <- data_t2$right_pos[i - 1]
  } else if (data_t2$dir[i] == "W") {
    data_t2$w_up[i] <- prev_w_up
    data_t2$w_right[i] <- prev_w_right - data_t2$val[i]
    data_t2$up_pos[i] <- data_t2$up_pos[i - 1]
    data_t2$right_pos[i] <- data_t2$right_pos[i - 1]
  } else {
    data_t2$up_pos[i] <- data_t2$up_pos[i - 1]
    data_t2$right_pos[i] <- data_t2$right_pos[i - 1]
    turns <-
      ifelse(data_t2$dir[i] == "R", data_t2$val[i] / 90, 4 - (data_t2$val[i] / 90))
    if (turns == 1) {
      data_t2$w_up[i] <- -prev_w_right
      data_t2$w_right[i] <- prev_w_up
    } else if (turns == 2) {
      data_t2$w_up[i] <- -prev_w_up
      data_t2$w_right[i] <- -prev_w_right
    } else if (turns == 3) {
      data_t2$w_up[i] <- prev_w_right
      data_t2$w_right[i] <- -prev_w_up
    }
  }
}

# Extract the final positions
t2 <- abs(data_t2$up_pos[nrow(data_t2)]) + abs(data_t2$right_pos[nrow(data_t2)])
