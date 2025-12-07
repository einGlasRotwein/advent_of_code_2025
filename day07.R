day07 <- readLines("./inputs/input_07.txt")
day07 <- strsplit(day07, "")
start_pos <- which(day07[[1]] == "S")
day07 <- day07[-1]
# remove empty lines
day07 <- day07[sapply(day07, function(x) !all(x == "."))]

options(scipen = 999)

get_splits <- function(beam_positions, next_row) {
  continue <- beam_positions[next_row[beam_positions] != "^"]
  splits <- beam_positions[next_row[beam_positions] == "^"]
  new_pos <- c(continue, c(splits - 1, splits + 1))
  new_pos <- unique(new_pos)
  return(list(new_pos = new_pos, n_splits = length(splits)))
}

get_splits2 <- function(beam_positions, next_row, pos_counts) {
  continue <- beam_positions[next_row[beam_positions] != "^"]
  splits <- beam_positions[next_row[beam_positions] == "^"]
  new_pos <- c(continue, c(splits - 1, splits + 1))
  
  temp_splits <- c(splits - 1, splits + 1)
  
  new_pos_counts <- 
  by(
    c(rep(pos_counts[as.character(splits)], 2), 
    pos_counts[as.character(continue)] 
  ),
  c(temp_splits, continue),
  sum
)

new_pos <- sort(unique(new_pos))
return(list(new_pos = new_pos, pos_counts = new_pos_counts))
}

## PART 1 -------------------------------------------------------------

start <- Sys.time()

split_counter <- 0
temp_pos <- start_pos

for (i in seq_along(day07)) {
  temp_results <- get_splits(temp_pos, day07[[i]])
  temp_pos <- temp_results$new_pos
  split_counter <- split_counter + temp_results$n_splits
}

Sys.time() - start

split_counter

## PART 2 -------------------------------------------------------------

start <- Sys.time()

temp_pos <- start_pos
temp_pos_counts <- 1
names(temp_pos_counts) <- start_pos

for (i in seq_along(day07)) {
  temp_results <- get_splits2(temp_pos, day07[[i]], temp_pos_counts)
  temp_pos <- temp_results$new_pos
  temp_pos_counts <- temp_results$pos_counts
}

Sys.time() - start

sum(temp_results$pos_counts)
