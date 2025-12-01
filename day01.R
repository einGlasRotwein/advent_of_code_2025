day01 <- readLines("inputs/input_01.txt")
day01 <- gsub("R", "", day01)
day01 <- as.numeric(gsub("L", "-", day01))

## PART 1 -------------------------------------------------------------

start <- Sys.time()

# start position: 50
sum(cumsum(c(50, day01)) %% 100 == 0)
# 1191

Sys.time() - start

## PART 2 -------------------------------------------------------------

start <- Sys.time()

pos <- 50
zeroes <- 0

for (r in seq_along(day01)) {

  zeroes <- zeroes + sum(seq(pos, pos + day01[r], ifelse(pos - pos + day01[r] < 0, -1, 1)) %% 100 == 0)
  if (pos == 0) zeroes <- zeroes - 1
  pos <- (pos + day01[r]) %% 100

}

zeroes
# 6858

Sys.time() - start

# fuck this shit, that doesn't work
# for (r in seq_along(day01)) {
#   # day01[r]
#   step <- (pos + day01[r]) / 100

#   if (step <= 0 | step >= .99) zeroes <- zeroes + ifelse(step < 0, ceiling(abs(step)), floor(step))
#   # if (pos != 0 & step <= 0 | step >= .99) zeroes <- zeroes + ifelse(step < 0, ceiling(abs(step)), floor(step))

#   pos <- (pos + day01[r]) %% 100
# }

# zeroes
# # 6835 - too low
