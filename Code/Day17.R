library(tidyverse)

# Read data and clean up
init <- read_lines("Data/Day17Test.txt") %>%
  sapply(., function(line) str_split(line, "")) %>% 
  unlist()

# Create expanded dataframe of inactive cubes
side <- sqrt(length(init)) + 6 * 2
cubes <- expand.grid(seq(1:side), seq(1:side), seq(-floor(side/2), floor(side/2))) %>% 
  as_tibble()
  # rename(x = Var1, y = Var2, z = Var3)

# Create expanded initial data
# Most confusing thing was to set the origin at the top left... helps me understand.
init_seq <- seq(from = 1, to = sqrt(length(init)), by = 1)
centre_inds <- expand.grid(init_seq, init_seq, 0) %>% 
  as_tibble() %>% 
  arrange(Var1) %>% 
  mutate(val = init)

# Join on initial values
data <- left_join(cubes, centre_inds, by = c("Var1", "Var2", "Var3")) %>% 
  replace_na(list(val = ".")) %>% 
  rename(x = Var1, y = Var2, z = Var3)


# Task 1 ------------------------------------------------------------------

get_new_val <- function(data, x_val, y_val, z_val) {
  
  current_state <- data %>%
    filter(x == x_val) %>% 
    filter(y == y_val) %>% 
    filter(z == z_val) %>% 
    pull(val)
  
  neighbours <- data %>%
    filter(x %in% seq(x_val - 1, x_val + 1)) %>% 
    filter(y %in% seq(y_val - 1, y_val + 1)) %>% 
    filter(z %in% seq(z_val - 1, z_val + 1)) %>% 
    filter(!(x == x_val & y == y_val & z == z_val)) %>% 
    group_by(val) %>%
    summarise(count = n())
  
  n_active <- neighbours %>% filter(val == "#") %>% pull(count)
  n_inactive <- neighbours %>% filter(val == ".") %>% pull(count)
  
  ifelse(current_state == ".",
         ifelse(n_active == 3, "#", "."),
         ifelse(n_active %in% c(2, 3), "#", "."))
  
}
