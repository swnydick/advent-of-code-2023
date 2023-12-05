#####################
# AoC R Solution 05 #
#                   #
# Steven W. Nydick  #
# 2023-12-05        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("05", "05-input.txt"))

# 2. Functions =================================================================

# extract the seeds from the original data
extract_seeds <- function(x){
  x <- regmatches(
    x = x,
    m = gregexec(
      text = x,
      pattern = "[0-9 ]+"
    )
  )
  x <- strsplit(unlist(x), " ")
  x <- as.numeric(unlist(x))
  x[!is.na(x)]
}

# extract the maps/pre-process
extract_maps <- function(x){
  flg  <- grep(x       = x,
               pattern = "map")

  if(length(flg) != 1){
    stop("more than one map name provided")
  }

  # pulling out from and to (might need it later??)
  map_type  <- gsub(x           = x[flg],
                    pattern     = " map.*$",
                    replacement = "") |>
               strsplit("\\-") |>
               unlist()
  map_type  <- list(from = head(map_type, 1),
                    to   = tail(map_type, 1))

  # pulling out the values
  map_vals  <- read.table(
    text   = c("to from len",
               x[-seq_len(flg)]),
    header = TRUE
  )

  list(type   = map_type,
       values = map_vals)
}

# single iteration through one map
find_next_map <- function(x, map,
                          is_rev_map = FALSE){

  # pulling out the values and the next place to go
  map_vals  <- map$values
  next_name <- map$type$to

  # pull out from to and len
  len       <- map_vals$len
  if(isTRUE(is_rev_map)){
    next_name <- map$type$from
    from      <- map_vals$to
    to        <- map_vals$from
  } else{
    next_name <- map$type$to
    from      <- map_vals$from
    to        <- map_vals$to
  }

  # indicate the distance using x
  dist      <- x - from
  flg       <- dist >= 0 & dist < len

  next_val <- x
  if(sum(flg) >= 2){
    stop("multiple map rows work")
  } else if(sum(flg) == 1){
    next_val <- to[flg] + dist[flg]
  }

  list(next_name = next_name,
       next_val  = next_val)
}

find_full_map <- function(x, maps,
                          is_rev_map = FALSE){

  # this is where we are going
  next_name   <- "seed"
  end_name    <- "location"
  all_names   <- sapply(maps, \(x) x$type$from)

  if(isTRUE(is_rev_map)){
    tmp_name  <- next_name
    next_name <- end_name
    end_name  <- tmp_name
    all_names <- sapply(maps, \(x) x$type$to)
  }

  # repeat the lookup until we reach location
  while(next_name != end_name){
    this_map  <- which(all_names == next_name)
    this_next <- find_next_map(x   = x,
                               map = maps[[this_map]],
                               is_rev_map = is_rev_map)
    x         <- this_next$next_val
    next_name <- this_next$next_name
  }

  return(x)
}

# 3. Part 1 ====================================================================

# splitting the vector by empty lines that separate the maps
maps  <- split(x = inp,
               f = cumsum(inp == ""))

# processing the seeds and the maps
seeds <- extract_seeds(maps[[1]])
maps  <- lapply(X   = maps[-1],
                FUN = extract_maps)

# finding the location given the seeds
loc   <- vapply(X         = seeds,
                FUN       = find_full_map,
                FUN.VALUE = numeric(1),
                maps      = maps)

min(loc)

# 4. Part 2 ====================================================================

# this is a minimization problem ... brute force strategy (because i'm stuck):
# - map backwards
# - stop as soon as a location hits a seed!
seed_from <- seeds[seq(1, length(seeds), by = 2)]
seed_len  <- seeds[seq(2, length(seeds), by = 2)]

for(this_loc in 45089205:min(loc)){
  this_seed <- find_full_map(x          = this_loc,
                             maps       = maps,
                             is_rev_map = TRUE)

  this_diff <- this_seed - seed_from
  has_map   <- this_diff >= 0 & this_diff < seed_len

  if(sum(has_map) >= 2){
    stop("multiple map rows work")
  } else if(sum(has_map) == 1){
    final_map <- this_loc
    break;
  }
}

# a better way would be to determine the [seed - location - len map]
