#####################
# AoC R Solution 07 #
#                   #
# Steven W. Nydick  #
# 2023-12-07        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- setNames(object = read.table(file.path("07", "07-input.txt")),
                nm     = c("cards", "bid"))

# 2. Functions =================================================================

find_card_ranks <- function(x,
                            has_jokers = FALSE){

  # determining the number of cards to iterate through
  card_rank <- c(2:9, "T", "J", "Q", "K", "A")

  if(isTRUE(has_jokers)){
    card_rank <- union("J", card_rank)
  }

  len       <- max(nchar(x))

  # setting an initial rank for overall
  hand_rank <- 0

  for(i in seq_len(len)){

    # starting from the last digit (lowest multiple) to the first (highest multiple)
    this_idx  <- len - i + 1
    this_card <- substr(x, this_idx, this_idx)

    # finding the specific rank based on the card rank
    this_rank <- match(this_card, card_rank) - 1

    # adding the multiple of the power (from n to 1) ensures that
    # - each addition is assigned to a unique number
    # - the numbers are sorted from low to high from right to left
    hand_rank <- hand_rank + this_rank * length(card_rank)^(i - 1)
  }

  return(hand_rank)
}

find_type_rank <- function(x,
                           has_jokers = FALSE){

  if(isTRUE(has_jokers)){
    n_jokers <- sum(x["J"], na.rm = TRUE)

    if(n_jokers > 0 && n_jokers < 5){
      x    <- x[!(names(x) %in% "J")]
      f    <- which.max(x)
      x[f] <- x[f] + n_jokers
    }
  }

  # fix the oddities (that won't get sorted correctly by taking the maximum)
  if(sum(x == 2) == 2){
    x <- 2.5
  } else if(any(x %in% 2) && any(x %in% 3)){
    x <- 3.5
  }

  max(x)
}

find_type_ranks <- function(x,
                            has_jokers = FALSE){
  count_x <- lapply(X   = strsplit(x, ""),
                    FUN = \(v) table(v))
  sapply(X   = count_x,
         FUN = find_type_rank,
         has_jokers = has_jokers)
}

# 3. Part 1 ====================================================================

cards     <- inp$cards
bid       <- inp$bid

# find the type ranks and the card ranks for each hand
type_rank <- find_type_ranks(cards)
card_rank <- find_card_ranks(cards)

# order the bid by card rank and type rank and multiply
sum(seq_along(bid) * bid[order(type_rank, card_rank)])


# 4. Part 2 ====================================================================

# find the type ranks and the card ranks for each hand (with jokers)
type_rank <- find_type_ranks(cards, TRUE)
card_rank <- find_card_ranks(cards, TRUE)

# order the bid by card rank and type rank and multiply
sum(seq_along(bid) * bid[order(type_rank, card_rank)])
