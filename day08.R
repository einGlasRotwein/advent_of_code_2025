day08 <- readLines("./inputs/input_08.txt")
day08 <- lapply(strsplit(day08, ","), as.numeric)

# step_goal <- 10 # example
step_goal <- 1000 # full input

# coords1 and coords2 each consist of x, y ,z
euclidean_dist <- function(coords1, coords2) {
  return(sqrt(sum((coords2 - coords1)^2)))
}

## PART 1 -------------------------------------------------------------

start <- Sys.time()

# matrix: all possible connections, and their distances. Only need 
# the stuff below the diagonal.
boxes <- matrix(NA, ncol = length(day08), nrow = length(day08))

for (x in 1:nrow(boxes)) {
  for (y in (1 + x - 1):ncol(boxes)) {
    boxes[x, y] <- euclidean_dist(day08[[x]], day08[[y]])
  }
}

for (i in 1:step_goal) {
  # find boxes clostest together (only look at values > 0)
  closest <- which(boxes == min(boxes[boxes > 0], na.rm = TRUE), arr.ind = TRUE)
  
  # check whether at least one of the boxes is already part of a circuit 
  # box 1
  circuits1 <- c(
    boxes[closest[1], ][boxes[closest[1], ] < 0],
    boxes[, closest[1]][boxes[ , closest[1]] < 0]
  )
  
  circuits1 <- unique(circuits1[!is.na(circuits1)])
  
  # box 2
  circuits2 <- c(
    boxes[closest[2], ][boxes[closest[2], ] < 0],
    boxes[, closest[2]][boxes[ , closest[2]] < 0]
  )
  
  circuits2 <- unique(circuits2[!is.na(circuits2)])
  
  if (length(c(circuits1, circuits2)) == 2) {
    # if both boxes are in the same circuit: skip
    # (i.e., set connection to 0, so we don't look at 
    # it again)
    if (circuits1 == circuits2) {
      boxes[closest] <- 0
      # if the boxes are part of two different circuits: join them
    } else {
      boxes[closest] <- circuits1
      boxes[boxes == circuits2 & !is.na(boxes)] <- circuits1
    }
    
    # if only one box i part of a circuit: assign it's id
  } else if (length(c(circuits1, circuits2)) == 1) {
    boxes[closest] <- c(circuits1, circuits2)
  } else {
    # otherwise, assign unused (negative) ID
    boxes[closest] <- min(boxes, na.rm = TRUE) - 1
  }
}

# get list of all boxes within a circuit ID
circuits <- table(boxes[boxes < 0 & !is.na(boxes)])

prod(
  sort(
    sapply(names(circuits), function(x) {
      length(unique(as.vector(which(boxes == x, arr.ind = TRUE))))
    }),
    decreasing = TRUE
  )[1:3]
)

Sys.time() - start

## PART 2 -------------------------------------------------------------

start <- Sys.time()

# continue where we left off after part 1 :-)
# go on until all boxes are connected in a single circuit, i.e.:
# - 1) there is only 1 circuit ID
# - 2) there are no boxes without a circuit ID

# but at least for the example, it's enough to check 2)
n_connections <- 
sapply(1:length(day08), function(x) {
  sum(c(boxes[x, ] < 0, boxes[ , x] < 0), na.rm = TRUE)
})

while (any(n_connections == 0)) {
  # find boxes clostest together (only look at values > 0)
  closest <- which(boxes == min(boxes[boxes > 0], na.rm = TRUE), arr.ind = TRUE)
  
  # check whether at least one of the boxes is already part of a circuit 
  # box 1
  circuits1 <- c(
    boxes[closest[1], ][boxes[closest[1], ] < 0],
    boxes[, closest[1]][boxes[ , closest[1]] < 0]
  )
  
  circuits1 <- unique(circuits1[!is.na(circuits1)])
  
  # box 2
  circuits2 <- c(
    boxes[closest[2], ][boxes[closest[2], ] < 0],
    boxes[, closest[2]][boxes[ , closest[2]] < 0]
  )
  
  circuits2 <- unique(circuits2[!is.na(circuits2)])
  
  if (length(c(circuits1, circuits2)) == 2) {
    # if both boxes are in the same circuit: skip
    # (i.e., set connection to 0, so we don't look at 
    # it again)
    if (circuits1 == circuits2) {
      boxes[closest] <- 0
      # if the boxes are part of two different circuits: join them
    } else {
      boxes[closest] <- circuits1
      boxes[boxes == circuits2 & !is.na(boxes)] <- circuits1
    }
    
    # if only one box i part of a circuit: assign it's id
  } else if (length(c(circuits1, circuits2)) == 1) {
    boxes[closest] <- c(circuits1, circuits2)
  } else {
    # otherwise, assign unused (negative) ID
    boxes[closest] <- min(boxes, na.rm = TRUE) - 1
  }
  
  n_connections <- 
  sapply(1:length(day08), function(x) {
    sum(c(boxes[x, ] < 0, boxes[ , x] < 0), na.rm = TRUE)
  })
}

day08[[closest[1]]][1] * day08[[closest[2]]][1]

Sys.time() - start
