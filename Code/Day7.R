library(tidyverse)

# Read data and clean up
data <- read_lines("Data/Day7.txt") %>%
  as_tibble() %>%
  separate(value,
           into = c("outer_bag", "inner_bag"),
           sep = " bags contain ") %>%
  mutate(inner_bag = str_split(inner_bag, "bags|bag")) %>%
  unnest_longer(inner_bag)  %>%
  mutate(
    inner_bag = trimws(str_remove(inner_bag, ","))
  ) %>%
  filter(inner_bag != ".") %>%
  mutate(
    inner_qty = parse_number(inner_bag, na = "no other"),
    inner_colour = if_else(
      is.na(inner_qty),
      NA_character_,
      trimws(str_remove(inner_bag, as.character(inner_qty)))
    )
  )

# Task 1 ------------------------------------------------------------------

# Create recursive function to get outer bags
get_outer_bags <- function(bag_colour, data) {
  outer <-
    data %>% filter(inner_colour == bag_colour, !is.na(inner_qty))
  
  # Recursive call for all new bags
  next_outer <-
    parallel::mclapply(outer$outer_bag, get_outer_bags, data)
  
  # Bind together to long dataframe
  bind_rows(outer, next_outer)
}

# Call recursive function and get unique outer bags
t1_ans <- get_outer_bags("shiny gold", data) %>%
  select(outer_bag) %>%
  unique()

# Task 2 ------------------------------------------------------------------

# Create recursive function to get inner bags
get_inner_bags <- function(bag_colour, data) {
  # Filter dataframe for inner bags, with valid quantity
  inner <-
    data %>% filter(outer_bag == bag_colour, !is.na(inner_qty))
  
  # Repeat rows based on quantity
  inner_rep <-
    as_tibble(lapply(inner, rep, inner$inner_qty))
  
  # Recursive call for all new bags
  next_inner <-
    parallel::mclapply(inner_rep$inner_colour, get_inner_bags, data)
  
  # Bind together to long dataframe
  bind_rows(inner_rep, next_inner)
}

# Call recursive function
t2_ans <- nrow(get_inner_bags("shiny gold", data))
