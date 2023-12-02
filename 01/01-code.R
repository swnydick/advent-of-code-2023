#####################
# AoC R Solution 01 #
#                   #
# Steven W. Nydick  #
# 2023-12-01        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- scan(
  file = file.path("01", "01-input.txt"), # change based on new folder
  what = character(),                     # change based on encoding
  blank.lines.skip = FALSE
)

# 2. Part 1 ====================================================================

# function to do part 1 because we need for part 2
add_first_two_digits <- function(inp){

  # keep only first and last digit
  numbers    <- gsub(
    x           = inp,
    pattern     = "[^0-9]",
    replacement = ""
  )

  # determine the number of numbers
  n_num      <- nchar(numbers)

  # extracting the first and last and adding
  two_digits <- paste0(
    substr(numbers, 1, 1),
    substr(numbers, n_num, n_num)
  )

  sum(as.numeric(two_digits))
}

add_first_two_digits(inp)

# 3. Part 2 ====================================================================

# function to reverse a string (annoying because there's nothing built in :/)
rev_str <- function(x){
  sapply(lapply(strsplit(x, ""), rev), paste, collapse = "")
}

# need to fix the input by replacing spelled numbers
num_map     <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
num_pat     <- paste0(num_map, collapse = "|")

# reverse the input and the pattern
num_pat_rev <- rev_str(num_pat)
inp_rev     <- rev_str(inp)

# pulling out the first and last number
first_num   <- regmatches(
  x  = inp,
  m  = regexpr(text    = inp,
               pattern = paste0(num_pat, "|[0-9]"))
)
last_num    <- regmatches(
  x  = inp_rev,
  m  = regexpr(text    = inp_rev,
               pattern = paste0(num_pat_rev, "|[0-9]"))
)

# note: last number is reversed so need to flip back the right way!
last_num    <- rev_str(last_num)

# combining the digits and replacing the spelled numbers
comb_num    <- list(first = first_num,
                    last  = last_num)
comb_num    <- lapply(
  X   = comb_num,
  FUN = function(n){
    flg    <- n %in% num_map
    n[flg] <- seq_along(num_map)[match(n[flg], num_map)]
    return(n)
  }
)

# pasting everything together and adding
sum(as.numeric(do.call(paste0, comb_num)))
