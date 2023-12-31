#####################
# AoC R Solution 02 #
#                   #
# Steven W. Nydick  #
# 2023-12-02        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("02", "02-input.txt"))

# 2. Functions =================================================================

find_color_max <- function(x,
                           type = "green"){

  # find the number with the color after it
  counts <- regmatches(
    x = x,
    m = gregexpr(
      text    = x,
      pattern = paste0("[0-9]+(?= ", type, ")"),
      perl    = TRUE
    )
  )

  # find the max value for the number (needs to be less than)
  sapply(counts, \(x) max(as.numeric(x)))
}

# 3. Part 1 ====================================================================

# find the maximum observed and allowed values for each set within each game
max_obs <- lapply(X   = c("red", "green", "blue"),
                  FUN = find_color_max,
                  x   = inp)
max_alw <- c(12, 13, 14)

# flag where we have enough for all colors
flag    <- Reduce(
  f = `&`,
  x = Map(
    f  = `<=`,
    e1 = max_obs,
    e2 = max_alw
  )
)

# add up their indices
sum(which(flag))

# 4. Part 2 ====================================================================

# find the sum of the product of the max cubes of each color needed
sum(Reduce(f = `*`, x = max_obs))
