day04 <- readLines("./inputs/input_04.txt")
day04 <- do.call(rbind, strsplit(day04, ""))

## FUNCTIONS ----------------------------------------------------------

get_neighbours <- function(pos, rows, cols) {
  neighbours <- 
  c(
    ifelse(pos %% rows != 1, pos - 1, NA), # t
    ifelse(pos <= (cols - 1) * rows & pos %% rows != 1, pos - 1 + rows, NA),
    ifelse(pos <= (cols - 1) * rows, pos + rows, NA), # r
    ifelse(pos %% rows != 0 & pos <= (cols - 1) * rows, pos + 1 + rows, NA), # dr
    ifelse(pos %% rows != 0, pos + 1, NA), # d
    ifelse(pos %% rows != 0 & pos > rows, pos + 1 - rows, NA), # dl
    ifelse(pos > rows, pos - rows, NA), # l
    ifelse(pos %% rows != 1 & pos > rows, pos - 1 - rows, NA) # tl
  )
  
  return(neighbours[!is.na(neighbours)])
}

count_paper_neighbours <- function(paper_pos, m) {
  neighbours <- get_neighbours(paper_pos, nrow(m), ncol(m))
  return(sum(m[neighbours] == "@"))
}

## PART 1 -------------------------------------------------------------

start <- Sys.time()

paper_positions <- which(day04 == "@")
accessible_count <- 0

for (i in paper_positions) {
  accessible_count <- accessible_count + (count_paper_neighbours(i, day04) < 4)
}

Sys.time() - start

accessible_count

## PART 2 -------------------------------------------------------------

start <- Sys.time()

n_removed <- 0
accessible_idx <- TRUE

while (sum(accessible_idx) > 0) {

  accessible_idx <- matrix(FALSE, nrow(day04), ncol(day04))

  for (i in paper_positions) {
    accessible_idx[i] <- count_paper_neighbours(i, day04) < 4
  }
  
  n_removed <- n_removed + sum(accessible_idx)
  
  day04[accessible_idx] <- "."
  paper_positions <- which(day04 == "@")

}

Sys.time() - start

n_removed
