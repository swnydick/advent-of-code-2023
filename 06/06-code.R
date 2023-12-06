#####################
# AoC R Solution 06 #
#                   #
# Steven W. Nydick  #
# 2023-12-06        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("06", "06-input.txt"))

# 2. Functions =================================================================

process_line <- function(x){
  x  <- strsplit(x, "\\:* +")[[1]]
  nm <- tolower(x[1])
  vl <- data.frame(as.numeric(x[-1]))
  setNames(vl, nm)
}

## A. Brute Force --------------------------------------------------------------

# note: you can use quadratic formula as well!
find_log_dist_by_speed <- function(time){
  speed <- log(0:time)
  speed + rev(speed)
}

count_winning_speeds_bf <- function(time, best_dist){
  cur_log_dist <- find_log_dist_by_speed(time)
  length(cur_log_dist) - sum(cur_log_dist <= log(best_dist))
}

## B. Quadratic Formula --------------------------------------------------------

count_winning_speeds_qf <- function(time, best_dist){
  # formula: t * (n - t) = dist
  # -> -t^2 + time * t - dist = 0
  # -> [-time +/- sqrt(time^2 - 4*dist) / [-2]
  x <- (time + c(-1, 1) * sqrt(time^2 - 4 * best_dist)) / 2
  floor(max(x)) - ceiling(min(x)) + 1
}

# 3. Part 1 ====================================================================

# process the input to turn it into a data.frame
goals <- do.call(
  what = cbind,
  args = lapply(
    X   = inp,
    FUN = process_line
  )
)

# count winning speeds for each dist (using both methods)
prod(mapply(count_winning_speeds_bf, goals$time, goals$distance))
prod(mapply(count_winning_speeds_qf, goals$time, goals$distance))

# 4. Part 2 ====================================================================

# update to put time and distance together
new_goal <- sapply(X   = goals,
                   FUN = paste,
                   collapse = "") |>
            as.numeric()

# count the winning speeds (taking logs prevents overflow!)
count_winning_speeds_bf(time      = new_goal[1],
                        best_dist = new_goal[2])
count_winning_speeds_qf(time      = new_goal[1],
                        best_dist = new_goal[2])
