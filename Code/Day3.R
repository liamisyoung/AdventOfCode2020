library(tidyverse)

# Read data (csv then convert to matrix)
path <- readr::read_csv("~/AdventOfCode2020/Data/Day3.txt", col_names = FALSE) %>%
  as.matrix()

# Ive merged both tasks in to one as it made sense to do so.

# List of slopes for task 2
slopes <- list(c(1,1),
               c(3,1),
               c(5,1),
               c(7,1),
               c(1,2))

# Write function to get number of trees
get_trees <- function(path, right, down) {
  # Pre filter on path to keep only rows accessed by trip
  valid_rows <- seq(from = 1,
                    to = length(path),
                    by = down)
  path <- path[valid_rows]

  trees <- 0
  # Loop across all rows
  for (i in 2:length(path)) {
    # Read string for row
    string <- path[i]

    # Tiny bit of math to repeat the loop of trees
    x_pos <- (i - 1) * right + 1 - (((i - 1) * right) %/% 31) * 31

    # Get the character in the position
    space <- substr(string, x_pos, x_pos)

    # If it's a tree, add a tree
    if (space == "#") {
      trees = trees + 1
    }
  }
  return(trees)
}

# Apply function over all slopes
trees_list <- sapply(slopes, function(coords) get_trees(path, coords[1], coords[2]))

# Answer for task 1 is the second slope in list
t1_ans <- trees_list[2]

# Answer for task 2 is product of all trees
t2_ans <- prod(trees_list)



