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

# 3. Part 1 ====================================================================

## A. Functions for Part 1 -----------------------------------------------------

# single iteration through one map
find_next_map <- function(x, map){

  # pulling out the values and the next place to go
  map_vals  <- map$values
  next_name <- map$type$to

  # pull out from to and len
  len       <- map_vals$len
  from      <- map_vals$from
  to        <- map_vals$to

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

find_full_map <- function(x, maps){

  # this is where we are going
  next_name   <- "seed"
  end_name    <- "location"
  all_names   <- sapply(maps, \(x) x$type$from)

  # repeat the lookup until we reach location
  while(next_name != end_name){
    this_map  <- which(all_names == next_name)
    this_next <- find_next_map(x   = x,
                               map = maps[[this_map]])
    x         <- this_next$next_val
    next_name <- this_next$next_name
  }

  return(x)
}

## B. Algorithm for Part 1 -----------------------------------------------------

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

## A. Functions for Part 2 -----------------------------------------------------

merge_maps <- function(m1, m2){

  m1     <- m1[order(m1$to), ]
  m2     <- m2[order(m2$from), ]

  # pull everything out
  from_1 <- m1$from; to_1 <- m1$to; len_1 <- m1$len
  from_2 <- m2$from; to_2 <- m2$to; len_2 <- m2$len

  # set space for new maps and the starting index for c
  from_c <- to_c <- len_c <- NULL
  c_idx  <- 1

  for(idx in seq_along(from_1)){
    from_1_i <- from_1[idx]; to_1_i <- to_1[idx]; len_1_i <- len_1[idx]

    # while we have length left for the map
    while(len_1_i > 0){

      # indicate the distance using the original from (same as find_next_map)
      dist <- to_1_i - from_2
      flg  <- dist >= 0 & dist < len_2

      # - if we are within the next map, then we use that map and mark the
      #   distance from the beginning of the map
      # - if we are not within next map, we use the original map as they are
      #   the same in this case and mark the distance to the next map up!
      if(sum(flg) >= 2){
        stop("multiple map rows work")
      } else if(sum(flg) == 1){
        shift    <- dist[flg]
        from_2_i <- from_2[flg]
        to_2_i   <- to_2[flg]
        left     <- len_2[flg] - shift
      } else{
        shift    <- 0
        from_2_i <- from_1_i
        to_2_i   <- to_1_i
        left     <- abs(dist[dist < 0])
      }

      # update the new map location based on the shift from the map start
      from_c[c_idx] <- from_2_i + shift
      to_c[c_idx]   <- to_2_i   + shift

      # the length is the minimum the length of the first map and the distance
      # we have left in map 2 until we are in a different map
      len_c[c_idx]  <- update <- min(len_1_i, left)

      # update the positions and index
      from_1_i      <- from_1_i + update
      to_1_i        <- to_1_i   + update
      len_1_i       <- len_1_i  - update
      c_idx         <- c_idx + 1
    }

  }

  # put everything into the same format as the input
  data.frame(from = from_c,
             to   = to_c,
             len  = len_c)
}

## B. Algorithm for Part 2 -----------------------------------------------------

# extract parts of the seeds
seed_from <- seeds[seq(1, length(seeds), by = 2)]
seed_len  <- seeds[seq(2, length(seeds), by = 2)]

# form the initial map that we will reduce across
seed_map  <- data.frame(from = seed_from,
                        to   = seed_from,
                        len  = seed_len)

# note: the map will be broken across break points of subsequent maps
#       therefore, the minimum of the final "to" maps on a break MUST be the
#       minimum possible to. therefore, map all of the ranges and find the
#       minimum to!

# note: we are going to simplify this by assuming that the maps are in the
#       correct order - if we don't want to do that we can apply a function
#       similar to the first part!
new_maps  <- lapply(X   = maps,
                    FUN = \(x) x$values)

min(Reduce(f = merge_maps, x = c(list(seed_map), new_maps))$to)
