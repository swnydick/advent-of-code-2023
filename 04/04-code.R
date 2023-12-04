#####################
# AoC R Solution 04 #
#                   #
# Steven W. Nydick  #
# 2023-12-03        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("04", "04-input.txt"))

# 2. Functions =================================================================

find_card_numbers <- function(x){
  lapply(strsplit(x, " +"), as.numeric)
}

count_card_matches <- function(x){
  sum(x[[2]] %in% x[[1]], na.rm = TRUE)
}

# 3. Part 1 ====================================================================

# remove "Card: " and split into "winning numbers" and "numbers you have"
all_cards <- gsub(x           = inp,
                  pattern     = "^.*\\: +",
                  replacement = "") |>
             strsplit(split = " *\\| *")

sep_cards <- lapply(X   = all_cards,
                    FUN = find_card_numbers)

# for each set of cards, determine the number of winning numbers
n_matches <- vapply(X   = sep_cards,
                    FUN = count_card_matches,
                    FUN.VALUE = numeric(1))

# based on count_card_matches, cards with 0 are NA to make it easier to count
sum(2^(n_matches - 1)[n_matches > 0], na.rm = TRUE)

# 4. Part 2 ====================================================================

# i don't know how to do this in a way that's not iterative :/

# pull out the number of matches, the total length, and the number of copies
n_o <- n_matches
n   <- length(n_o)
n_c <- rep(1, n)

for(card_idx in seq_len(n - 1)){

  # start with the count (these indicates the seq of cards that are won)
  this_n_o     <- n_o[card_idx]

  # - indicate the slice/sequence of card copies (based on n_i)
  # - update the number of copies for each card in that sequence
  this_sl      <- card_idx + seq_len(min(this_n_o, n))
  n_c[this_sl] <- n_c[this_sl] + n_c[card_idx]
}

# add all of the copies up!
sum(n_c)

# note: do this in a way that is more idiomatic R coding?
