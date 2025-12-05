day05 <- readLines("./inputs/input_05.txt")

options(scipen = 999)

# fresh ingredient ranges: before empty line
empty <- which(day05 == "")
fresh <- day05[1:(empty - 1)]
fresh1 <- as.numeric(sapply(strsplit(fresh, "-"), function(x) x[[1]]))
fresh2 <- as.numeric(sapply(strsplit(fresh, "-"), function(x) x[[2]]))
available <- as.numeric(day05[(empty + 1):length(day05)])

## PART 1 -------------------------------------------------------------

start <- Sys.time()

fresh_counter <- 0

for (i in available) {
  
  for (fresh_range in seq_along(fresh1)) {
    
    if (i >= fresh1[fresh_range] & i <= fresh2[fresh_range]) {
      fresh_counter <- fresh_counter + 1
      break
    }
    
    
  }
  
}

fresh_counter

Sys.time() - start

## PART 2 -------------------------------------------------------------

start <- Sys.time()

unique_ranges1 <- fresh1[1]
unique_ranges2 <- fresh2[1]

# for (i in 2:96) {
for (i in 2:length(fresh1)) {
  # consecutively cut out overlaps between ranges
  
  # for current range, get the range(s) that is/are not covered by any 
  # of the previous range(s)
  cut_range1 <- fresh1[i]
  cut_range2 <- fresh2[i]
  
  # for (j in 1:91) {
  for (j in seq_along(unique_ranges1)) {
    
    cut_range_counter <- 1
    
    while (cut_range_counter <= length(cut_range1)) {
      
      # lower limit within previous range
      lower_within <- cut_range1[cut_range_counter] >= unique_ranges1[j] & cut_range1[cut_range_counter] <= unique_ranges2[j]
      # upper limit within previous range
      upper_within <- cut_range2[cut_range_counter] >= unique_ranges1[j] & cut_range2[cut_range_counter] <= unique_ranges2[j]
      # old range entirely within new range
      old_within_new <- 
        unique_ranges1[j] >= cut_range1[cut_range_counter] & unique_ranges1[j] <= cut_range2[cut_range_counter] &
        unique_ranges2[j] >= cut_range1[cut_range_counter] & unique_ranges2[j] <= cut_range2[cut_range_counter]
      
      # if old range entirely within new range: cut it out of new range
      # i.e., replace current new range with two new ranges
      
      # edge case: perfect overlap - ditch new range
      if (
        unique_ranges1[j] == cut_range1[cut_range_counter] & 
        unique_ranges2[j] == cut_range2[cut_range_counter]
      ) {
        cut_range1 <- c()
        cut_range2 <- c()
        # if new range covered entirely by existing range: ditch the new range
      } else if (old_within_new) {
        
        # add 2 new ranges
        cut_range1 <- c(cut_range1, cut_range1[cut_range_counter], unique_ranges2[j] + 1)
        cut_range2 <- c(cut_range2, unique_ranges1[j] - 1, cut_range2[cut_range_counter])
        
        # delete old
        cut_range1 <- cut_range1[-cut_range_counter]
        cut_range2 <- cut_range2[-cut_range_counter]
        
        cut_range_counter <- cut_range_counter + 2
        # if new range covered entirely by existing range: ditch the new range
      } else if (lower_within & upper_within) {
        cut_range1 <- c()
        cut_range2 <- c()
      } else if (lower_within) {
        cut_range1 <- unique_ranges2[j] + 1
        cut_range_counter <- cut_range_counter + 1
      } else if (upper_within) {
        cut_range2 <- unique_ranges1[j] - 1
        cut_range_counter <- cut_range_counter + 1
      } else {
        cut_range_counter <- cut_range_counter + 1
      }
      
    }
    
  }
  
  # add cut range(s) to unique ranges
  unique_ranges1 <- c(unique_ranges1, cut_range1)
  unique_ranges2 <- c(unique_ranges2, cut_range2)
}

sum(unique_ranges2 - unique_ranges1 + 1)

Sys.time() - start
