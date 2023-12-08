#####################
# AoC R Solution 08 #
#                   #
# Steven W. Nydick  #
# 2023-12-08        #
#####################

options(scipen = 20)

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("08", "08-input.txt"))

# 2. Functions =================================================================

# turn steps from L/R into 1/2 for use in the algorithm
parse_steps <- function(x){
  match(unlist(strsplit(x, "")), c("L", "R"))
}

# split the map so that we can just follow the names appropriately
parse_map <- function(x){

  # splitting the current and the next steps
  x        <- strsplit(x     = inp[-(1:2)],
                       split = " *\\= *")

  # finding the current value of the map
  map_cur  <- vapply(X   = x,
                     FUN = \(v) v[[1]],
                     FUN.VALUE = character(1))

  # finding the next location to go to
  map_next <- vapply(X   = x,
                     FUN = \(v) v[[2]],
                     FUN.VALUE = character(1))
  map_next <- regmatches(
    x = map_next,
    m = gregexec(
      text    = map_next,
      pattern = "[A-Z]{3}"
    )
  )

  # putting everything as a matrix to make it faster
  setNames(object = lapply(map_next, c),
           nm     = map_cur) |>
  do.call(what = rbind,
          args = _)
}


# 3. Part 1 ====================================================================

## A. Functions ----------------------------------------------------------------

# function to rotate vector if we start in different locations (not needed)
rotate_vector <- function(x, n = 0){
  rng <- length(x)
  n   <- n - 1
  x[1 + (rng + (seq_along(x) + n) %% rng) %% rng]
}

find_camel_steps <- function(steps,
                             maps,
                             shift = 0,
                             first = "AAA",
                             last  = "ZZZ"){

  # find the total number of steps (so we can circle)
  n         <- length(steps)
  steps     <- rotate_vector(steps)

  # indicate the temporary variables
  next_loc  <- first  # the next place we iterate to
  cur_iter  <- 0      # the current interation
  end_loop  <- FALSE  # whether we leave the loop
  step_seq  <- first  # the current sequence in the step

  # update the location and determine if it matches the end point
  while(!end_loop){
    cur_iter               <- cur_iter + 1
    cur_step               <- steps[(cur_iter - 1) %% n + 1]
    next_loc               <- maps[next_loc, cur_step]
    step_seq[cur_iter + 1] <- next_loc
    end_loop               <- next_loc == last
  }

  # return the sequence
  step_seq
}

## B. Algorithm ----------------------------------------------------------------

# process inp to get the left/right instructions as well as the map
steps   <- parse_steps(inp[[1]])
maps    <- parse_map(inp[-(1:2)])

# count steps from first to last location
all_steps <- find_camel_steps(
  steps = steps,
  maps  = maps,
  first = "AAA",
  last  = "ZZZ"
)

# count up and remove the starting point
length(all_steps) - 1

# 4. Part 2 ====================================================================

## A. Functions ----------------------------------------------------------------

# greatest common divisor and least common multiple
gcd <- function(x, y){
  r <- x %% y;
  return(ifelse(r, gcd(y, r), y))
}

lcm <- function(x, y){
  (abs(x) * abs(y)) / gcd(x, y)
}

# the trick is that these are closed loops with no shift to start
find_camel_loops <- function(steps,
                             maps,
                             start = "ZZZZZ"){
  Map(
    f     = find_camel_steps,
    steps = list(steps),
    maps  = list(maps),
    first = start,
    last  = start
  )
}

# we will make an assumption that the loops:
# - are closed and repeat
# - start in the same place
# - are the same length every time

# given this assumption, we only worry about the end and not the beginning
end_loc  <- grep(
  x       = rownames(maps),
  pattern = "Z$",
  value   = TRUE
)

# find the lengths of the loops
loop_seq <- find_camel_loops(steps = steps,
                             maps  = maps,
                             start = end_loc)
loop_len <- lengths(loop_seq) - 1

# find the least common multiple of the lengths
Reduce(f = lcm,
       x = loop_len)

# note: without some of these assumptions, this would be trickier because
# - we'd have to account for the start of each loop (which could be past the A)
# - we'd need to know whether the loops are different based on the sequence
#   (the length from ZZZZZ to ZZZZZ could depend on the where in the rl set the
#    ZZZZZ is located)
# - the result turned out to be the simple way, so i didn't extend this to be
#   more complicated!

# for the more complicated way we would need to use the CRT and check loop
# length/consistency!
