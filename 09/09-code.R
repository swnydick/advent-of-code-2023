#####################
# AoC R Solution 09 #
#                   #
# Steven W. Nydick  #
# 2023-12-09        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("09", "09-input.txt"))

# 2. Functions =================================================================

sum_next_in_seq <- function(x,
                            last = TRUE){

  # determine how we recurse the algorithm
  if(isTRUE(last)){
    # last? iterate from last element and move up by adding!
    slice   <- tail
    combine <- `+`
  } else{
    # first? iterate from first element and move up by subtracting!
    slice   <- head
    combine <- `-`
  }

  # the recursive algorithm: find bottom, add up the tree!
  find_next <- function(x){
    if(all(x == 0)){
      return(0)
    } else{
      return(combine(slice(x, 1), find_next(diff(x))))
    }
  }

  # apply the algorithm to all sequences
  sum(sapply(x, find_next))
}


# 3. Part 1 ====================================================================

# extract the sequences from the input
cur_seq <- lapply(X   = strsplit(inp, " "),
                  FUN = as.numeric)

# update the last element and add them all together
sum_next_in_seq(cur_seq, TRUE)

# 4. Part 2 ====================================================================

# update the first element and add them all together
sum_next_in_seq(cur_seq, FALSE)
