library(tidyverse)

# Read data and clean up
data <- read_lines("Data/Day16.txt") 

inds <- which(data == "")

rules <- data[1:inds[1] - 1]  %>% 
  enframe(name = NULL) %>% 
  separate(value,
           into = c("name", "val"),
           sep = ": ") %>% 
  separate(val,
           into = c("rule_1", "rule2"),
           sep = " or ") %>% 
  separate(rule_1,
           into = c("rule_1_low", "rule_1_high"),
           sep = "-") %>% 
  separate(rule2,
           into = c("rule_2_low", "rule_2_high"),
           sep = "-") %>% 
  mutate(rule_1_low = as.integer(rule_1_low),
         rule_1_high = as.integer(rule_1_high),
         rule_2_low = as.integer(rule_2_low),
         rule_2_high = as.integer(rule_2_high))

my <- data[(inds[1] + 2)] %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer() %>% 
  as_tibble() %>% 
  t() %>% 
  as_tibble()

nearby <- data[(inds[2] + 2):length(data)]  %>% 
  str_split(",") %>% 
  lapply(as.integer) %>% 
  lapply(function(x) {as_tibble(t(enframe(x, name = NULL)), name = NULL)}) %>% 
  bind_rows()

# Task 1 ------------------------------------------------------------------

# Function to take ticket by ticket and check if there is an invalid value
check_ticket <- function(ticket) {
  valid <- matrix(nrow = length(ticket), ncol = nrow(rules))
  for (i in 1:nrow(rules)) {
    rule <- rules[i,]
    rule_valid <- sapply(ticket, function(val) {
      between(val, rule$rule_1_low, rule$rule_1_high) | between(val, rule$rule_2_low, rule$rule_2_high)
    })
    valid[i,] <- rule_valid
  }
  
  error_vals <- which(colSums(valid) == 0)
  
  # Return 0 if valid ticket, return the error value if invalid
  if (length(error_vals) == 0) {
    return(0)
  } else {
    return(ticket[error_vals] %>% pull())
  }
}

# Get sum of error values from invalid tickets
sum <- 0
for (i in 1:nrow(nearby)) {
  sum <- sum + check_ticket(nearby[i,])
}

# Task 2 ------------------------------------------------------------------

# Same function as above but return the input row if valid, or an empty tibble if invalid
validate_ticket <- function(ticket) {
  valid <- matrix(nrow = length(ticket), ncol = nrow(rules))
  for (i in 1:nrow(rules)) {
    rule <- rules[i,]
    rule_valid <- sapply(ticket, function(val) {
      between(val, rule$rule_1_low, rule$rule_1_high) | between(val, rule$rule_2_low, rule$rule_2_high)
    })
    valid[i,] <- rule_valid
  }
  
  error_vals <- which(colSums(valid) == 0)
  
  if (length(error_vals) == 0) {
    return(ticket)
  } else {
    return(ticket %>% slice(-1))
  }
}

# Filter for valid tickets
valid_tickets <- list()
for (i in 1:nrow(nearby)) {
  valid_tickets[[i]] <- validate_ticket(nearby[i,])
}
valid_tickets <- bind_rows(valid_tickets)

# Go through column by column and see which rules that column is valid for, writing to a list
valid_cols <- vector("list", length = ncol(valid_tickets))
for (i in 1:ncol(valid_tickets)) {
  col_vals <- valid_tickets[,i]
  names(col_vals) <- "value"
  
  for (j in 1:nrow(rules)) {
    rule <- rules[j,]
    
    col_vals <- col_vals %>% 
      mutate(
        !!paste0(rule$name) := between(value, rule$rule_1_low, rule$rule_1_high) |
          between(value, rule$rule_2_low, rule$rule_2_high)
      )
  }
  # valid_cols[[i]] will be a character vector of valid column names for value i in our ticket
  valid_cols[[i]] <- names(which(col_vals %>% select(-value) %>% colSums() == nrow(valid_tickets)))
}

# Most columns will have many rules it is valid for. If there is only one valid, that's the one.
# Set it as the column, then remove it from the rest. Repeat until only one is valid for each.
final_cols <- vector("character", length = length(valid_cols))
while (sum(final_cols == "") > 0) {
  singles <- which(sapply(valid_cols, length) == 1)
  final_cols[singles] <- valid_cols[singles]
    
  valid_cols <- lapply(valid_cols, function(x) {
    x[x != unlist(valid_cols[singles])]
  })
}

# Set the names to our ticket
names(my) <- unlist(final_cols)

# Get the relevant values for the answer
t2 <- my %>% select(starts_with("departure")) %>% unlist() %>% prod()