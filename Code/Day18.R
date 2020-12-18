library(tidyverse)

# Read data and clean up
data <- read_lines("Data/Day18.txt")

# Task 1 ------------------------------------------------------------------

# I was looking at overriding precedence in R and short answer is it's not possible

# So I started  writing a new_eval to add from l -> r and unnesting brackets blah blah blah
# Then I googled one more time... Just incase. Found out you can create new infix operators
# which have no order precedence. https://dev.to/awwsmm/operator-overloading-in-r-19fc
"%plus%" <- function(x, y) sum(x, y)
"%times%" <- function(x, y) prod(x, y)

eval_t1 <- function(expr) {
  # Replace functions with new ones
  str_replace_all(expr, "\\+", "%plus%") %>% 
    str_replace_all(., "\\*", "%times%") %>% 
    # Parse as text
    parse(text = .) %>% 
    # Evaluate
    eval()
}

# Get answer
t1 <- sapply(data, eval_t1) %>% sum()

# Task 2 ------------------------------------------------------------------

# Better yet (but dangerous) we can overwrite current ones. If we rename the + as *
# and * as + then the * will still evaluate first, but do an addition. Sneaky.
"*" <- function(x, y) sum(x, y)
"+" <- function(x, y) prod(x, y)

eval_t2 <- function(expr) {
  # Replace functions with new ones, need temp t not to overwrite all
  str_replace_all(expr, "\\+", "t") %>% 
    str_replace_all(., "\\*", "+") %>% 
    str_replace_all(., "t", "*") %>% 
    # Parse as text
    parse(text = .) %>% 
    # Evaluate
    eval()
}

# Get answer
t2 <- sapply(data, eval_t2) %>% sum()
