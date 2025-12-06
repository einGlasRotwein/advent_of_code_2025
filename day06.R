day06 <- readLines("./inputs/input_06.txt")
operators <- unlist(strsplit(gsub("\\s+", "", day06[length(day06)]), ""))

options(scipen = 999)

## PART 1 -------------------------------------------------------------

start <- Sys.time()

numbers <- do.call(rbind, lapply(strsplit(day06[-length(day06)], " "), function(x) as.numeric(x[x != ""])))

sum(
  apply(numbers[ , operators == "*"], 2, prod),
  apply(numbers[ , operators == "+"], 2, sum)
)

Sys.time() - start

## PART 2 -------------------------------------------------------------

start <- Sys.time()

# now split this into dfs at empty columns
m <- do.call(rbind, strsplit(day06, ""))
starts <- c(1, which(apply(m, 2, function(x) all(x == " "))) + 1)
ends <- c(which(apply(m, 2, function(x) all(x == " "))) - 1, ncol(m))

sublists <- vector("list", length = length(start))

for (i in seq_along(starts)) {
  sublists[[i]] <- m[ , starts[i]:ends[i]]
}

sum(
  sapply(sublists, function(x) {
    nums <- as.numeric(apply(t(x[-nrow(x), ]), 1, function(y) paste(y, collapse = "")))
    if (x[nrow(x)] == "+") {
      sum(nums)
    } else {
      prod(nums)
    }
  })
)

Sys.time() - start
