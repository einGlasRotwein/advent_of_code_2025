day02 <- readLines("inputs/input_02.txt")
# example
# day02 <- "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
split_nums <- strsplit(unlist(strsplit(day02, ",")), "-")
n1 <- sapply(split_nums, function(x) as.numeric(x[[1]]))
n2 <- sapply(split_nums, function(x) as.numeric(x[[2]]))

## PART 1 -------------------------------------------------------------

start <- Sys.time()

invalid_count <- 0

for (i in seq_along(n1)) {
  
  nums <- as.character(n1[i]:n2[i])
  # exclude all IDs with an uneven number of letters
  nums <- nums[nchar(nums) %% 2 == 0]
  first <- substr(nums, 1, nchar(nums) / 2)
  second <- substr(nums, nchar(nums) / 2 + 1, nchar(nums))
  invalid_count <- invalid_count + sum(as.numeric(nums[first == second]))
  
}

Sys.time() - start

invalid_count

## PART 2 -------------------------------------------------------------

start <- Sys.time()

invalid_count <- 0

for (i in seq_along(n1)) {
  
  nums <- as.character(n1[i]:n2[i])
  
  for (num in nums) {
    if (nchar(num) == 1) next

    # get divisors
    y <- seq_len( ceiling( nchar(num) / 2 ) )

    for (piece_length in y[nchar(num) %% y == 0]) {
        pieces <- strsplit(num, paste0("(?<=.{", piece_length,"})"), perl = TRUE)[[1]]
        
        if (length(unique(pieces)) == 1) {
          invalid_count <- invalid_count + as.numeric(num)
          break
        }
    }
  }
  
}

Sys.time() - start

invalid_count
