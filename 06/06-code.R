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

# note: you really only need to do time / 2 because it's symmetric
find_log_dist_by_speed <- function(time){
  speed <- log(0:time)
  speed + rev(speed)
}

# taking logs AND subtracting from N prevents overflow
count_winning_speeds <- function(time, best_dist){
  cur_log_dist <- find_log_dist_by_speed(time)
  length(cur_log_dist) - sum(cur_log_dist <= log(best_dist))
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

# count winning speeds for each dist
prod(mapply(count_winning_speeds, goals$time, goals$distance))

# 4. Part 2 ====================================================================

# update to put time and distance together
new_goal <- sapply(X   = goals,
                   FUN = paste,
                   collapse = "") |>
            as.numeric()

# count the winning speeds (taking logs prevents overflow!)
count_winning_speeds(time      = new_goal[1],
                     best_dist = new_goal[2])
