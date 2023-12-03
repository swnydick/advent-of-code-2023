#####################
# AoC R Solution 03 #
#                   #
# Steven W. Nydick  #
# 2023-12-02        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("03", "03-input.txt"))

# 2. Functions =================================================================

# finding all locations that are adjacent
find_adjacent_locs <- function(old_locs,
                               simplify = TRUE){

  # the locations are current plus all combinations of 0, -1, and 1
  opts <- c(0, -1, 1)
  cmbs <- t(combn(opts, 2))
  cmbs <- rbind(cmbs,
                cmbs[ , 2:1],
                cbind(opts, opts))

  # finding the valid locations based on the combinations
  locs <- apply(
    X        = cmbs,
    MARGIN   = 1,
    FUN      = \(x){
      data.frame(
        row = old_locs$row + x[1],
        col = old_locs$col + x[2]
      )
    },
    simplify = FALSE
  )

  # binding everything together
  if(isTRUE(simplify)){
    do.call(what = rbind,
            args = locs)
  } else{
    return(locs)
  }
}

# combining locations into a single string for comparisons
combine_locs <- function(locs){
  do.call(what = \(...) paste(..., sep = "-"),
          args = locs)
}

is_same_loc <- function(r, c){
  head(r, -1) == tail(r, -1) & head(c, -1) == (tail(c, -1) - 1)
}

# finding match index for all locations
find_match_index <- function(x, y, idx){
  x <- combine_locs(x)
  y <- combine_locs(y)
  idx[match(x, y)]
}

# 3. Part 1 ====================================================================

# turning everything into a matrix to make it easy to work with
scheme_mat <- do.call(what = rbind,
                      args = strsplit(inp, ""))

# find the location of everything that is numeric AND is a symbol
is_num     <- matrix(data = scheme_mat %in% 0:9,
                     nrow = nrow(scheme_mat))
is_sym     <- matrix(data = !(scheme_mat %in% "." | is_num),
                     nrow = nrow(scheme_mat))
is_star    <- matrix(data = scheme_mat %in% "*",
                     nrow = nrow(scheme_mat))

# determining the rows and column indices of the symbols and numbers
all_locs   <- lapply(
  X   = list(numbers = is_num,
             symbols = is_sym,
             stars   = is_star),
  FUN = \(x){
    data.frame(
      row = row(x)[x],
      col = col(x)[x]
    )
  }
)

## numbers ##

# find the number locations (in order)
num_locs      <- all_locs$numbers
num_locs      <- num_locs[do.call(what = order, args = num_locs), ]

# find the number index based on same row and col only one above
num_idx       <- cumsum(!c(FALSE, is_same_loc(num_locs$row, num_locs$col)))

# past the number values together based on index and convert to numeric
num_vals      <- tapply(
  X     = scheme_mat[as.matrix(num_locs)],
  INDEX = num_idx,
  FUN   = paste,
  collapse = ""
)

num_vals      <- as.numeric(num_vals)

## symbols ##

# find the symbol locations (order doesn't matter)
sym_locs      <- all_locs$symbols

# valid locations are: [row, col +/- 1], [row +/- 1, col], [row +/- 1, col +/- 1]
adjacent_locs <- find_adjacent_locs(sym_locs)

## extracting locations ##

# keeping the number indices that are valid and take the unique ones
valid_idx     <- num_idx[combine_locs(num_locs) %in% combine_locs(adjacent_locs)]
valid_idx     <- unique(valid_idx)

# adding all of the valid numbers
sum(num_vals[valid_idx])

# 4. Part 2 ====================================================================

# find the locations of the "*" (the gear symbols) and the adjacent ones
star_locs     <- all_locs$stars
adjacent_locs <- find_adjacent_locs(
  old_locs = star_locs,
  simplify = FALSE
)

# for each of the adjacent locs, find whether they match a number
match_idx     <- lapply(
  X   = adjacent_locs,
  FUN = find_match_index,
  y   = num_locs,
  idx = num_idx
)

# put each row into separate list elements
match_idx    <- do.call(
  what = Map,
  args = c(list(f = c), match_idx)
)

# combine everything and remove NAs and duplicates
match_idx    <- lapply(
  X   = match_idx,
  FUN = \(x) unique(x[!is.na(x)])
)

# keep only the index pair that are of length 2
valid_sets   <- do.call(
  what = rbind,
  args = match_idx[lengths(match_idx) == 2]
)
valid_sets   <- as.data.frame(valid_sets)

# adding all valid pairwise multiples
sum(Reduce(f = `*`, x = lapply(valid_sets, \(idx) num_vals[idx])))
