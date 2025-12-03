day03 <- readLines("inputs/input_03.txt")
day03 <- lapply(strsplit(day03, ""), as.numeric)
options(scipen = 999)

## FUNCTION -----------------------------------------------------------

# function to find max n-digit combo in vector
# can we find a 9? and a 9 after that? if not, an 8 after that ... ?
find_joltage <- function(x, n_digits) {
  vals <- 9:1
  jolt_idxs <- rep(NA, n_digits)
  
  for (i in 1:n_digits) {

    if (i == 1) {
      temp_vec <- x
    } else {
      temp_vec <- x[(jolt_idxs[i-1] + 1):length(x)]
    }
    
    for (val in vals) {

      temp_pos <- match(val, temp_vec)
      
      if (!is.na(temp_pos) & temp_pos <= (length(temp_vec) - sum(is.na(jolt_idxs)) + 1)) {
        if (all(is.na(jolt_idxs))) {
          jolt_idxs[i] <- temp_pos
        } else {
          jolt_idxs[i] <- jolt_idxs[i-1] + temp_pos
        }
        break
      }
      
    }
    
  }

  return(as.numeric(paste0(x[jolt_idxs], collapse = "")))
}

## PART 1 -------------------------------------------------------------

start <- Sys.time()

sum(sapply(day03, function(x) find_joltage(x, 2)))

Sys.time() - start

## PART 2 -------------------------------------------------------------

start <- Sys.time()

sum(sapply(day03, function(x) find_joltage(x, 12)))

Sys.time() - start