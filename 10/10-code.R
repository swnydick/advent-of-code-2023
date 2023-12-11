#####################
# AoC R Solution 10 #
#                   #
# Steven W. Nydick  #
# 2023-12-10        #
#####################

# 1. Setup =====================================================================

# read data into R (call it `inp` to reflect the file name)
inp <- readLines(file.path("10", "10-input.txt"))

# 2. Functions =================================================================

## A. Part 1 -------------------------------------------------------------------
follow_pipe <- function(type  = c("|", "-", "L", "J", "7", "F"),
                        move  = 1,
                        along = c("x", "y")){

  # x is along a given row (across columns)
  # y is along a given col (across rows)

  # note: one needs to be 0 at all times or else this won't work
  other_along <- c("x", "y")[(along == "x") + 1]

  if(type %in% c("-", "|")){
    # don't change anything
  } else if(type %in% c("L", "7")){
    # switch the axes but keep directions
    along <- other_along
  } else if(type %in% c("J", "F")){
    # reverse directions AND switch the axes
    move  <- -1 * move
    along <- other_along
  }

  list(move  = move,
       along = along)
}

traverse_pipes <- function(pipes,
                           start_x = 1,
                           start_y = 1,
                           move    = 1,
                           along   = c("x", "y")){

  # start at the "S" and treat the current as the starting
  count    <- 0
  cur_x    <- start_x
  cur_y    <- start_y
  pipe_seq <- NULL

  repeat{
    # pull the pipe, update the border, and find the next move
    type      <- pipes[cur_x, cur_y]
    pipes[cur_x, cur_y] <- "B"
    next_move <- follow_pipe(type  = type,
                             move  = move,
                             along = along)
    move      <- next_move$move
    along     <- next_move$along

    # update the current x/y location based on the move
    if(along == "x"){
      cur_y   <- cur_y + move
    } else{
      cur_x   <- cur_x + move
    }

    # update the sequence and count
    count           <- count + 1
    pipe_seq[count] <- type

    # leave if we've reached the beginning
    if(cur_x == start_x && cur_y == start_y){
      break
    }
  }

  return(list(pipe_seq = pipe_seq,
              count    = count,
              pipes    = pipes))
}

find_pipe_loop <- function(pipes){

  # find start of loop
  start_flag <- pipes == "S"
  start_x    <- row(pipes)[start_flag]
  start_y    <- col(pipes)[start_flag]

  # TERRIBLE CODE BECAUSE I'M STUCK
  if(pipes[start_x + 1, start_y] %in% c("|", "L", "J")){
    move  <- +1
    along <- "y"
    pipes[start_x, start_y] <- "|"
  } else if(pipes[start_x, start_y + 1] %in% c("-", "J", "7")){
    move  <- +1
    along <- "x"
    pipes[start_x, start_y] <- "-"
  } else if(pipes[start_x - 1, start_y] %in% c("|", "7", "F")){
    move  <- -1
    along <- "y"
    pipes[start_x, start_y] <- "|"
  }

  # run algorithm
  traverse_pipes(
    pipes   = pipes,
    start_x = start_x,
    start_y = start_y,
    move    = move,
    along   = along
  )
}

## B. Part 2 -------------------------------------------------------------------

flood_pipes <- function(pipes){

  # fct: flag if we're out of bounds
  flag_rc   <- function(i, j){
    i >= 1 & i <= n_r & j >= 1 & j <= n_c
  }

  # fct: pull the pipe value and replace with "Z" if out of bounds
  pull_pipe <- function(i, j){
    if(!flag_rc(i, j)){
      "Z"
    } else{
      pipes[i, j]
    }
  }

  # fct: paste idx together
  combine_idx <- function(i, j){
    paste(i, j, sep = "-")
  }

  # fct: translate rows/columns to idx
  find_idx <- function(i, j){
    match(combine_idx(i, j), cmb_idx)
  }

  # fct: find next idx based on current idx
  find_next_idx <- function(idx){
    # pulling out the rows and columns
    rw  <- row_idx[idx] + move_grid$r
    cl  <- col_idx[idx] + move_grid$c

    # flag if we're out of bounds and filter based on the flags
    fl  <- flag_rc(rw, cl)
    rw  <- rw[fl]
    cl  <- cl[fl]
    gr  <- move_grid[fl, ]

    # adjust the index based on flags and direction of travel
    find_idx(rw, cl)
    # idx <- Map(f    = adjust_idx,
    #            idx  = find_idx(rw, cl),
    #            mv_r = gr$r,
    #            mv_c = gr$c,
    #            adj  = list(c(-1, 1)))
  }

  # fct: adjust idx if we are in a border BUT if we can adjust
  adjust_idx <- function(idx,
                         mv_r,
                         mv_c,
                         adj = 0){

    if(loop_mark[idx] != "B" || (mv_r != 0 & mv_c != 0)){
      return(idx)
    }

    # pull out the row and column indices
    rw      <- row_idx[idx]
    cl      <- col_idx[idx]
    pipe_0  <- pipes[idx]
    new_idx <- NULL
    new_adj <- NULL

    if(mv_r != 0){
      pipe_m1 <- pull_pipe(rw, cl - 1)
      pipe_p1 <- pull_pipe(rw, cl + 1)
      if(pipe_0 %in% c("|", "L", "F") && -1 %in% adj){
        if(pipe_m1 %in% c("|", "J", "7")){
          if(pipe_mark[rw, cl - 1] == "B"){
            new_idx <- c(new_idx, find_idx(rw + mv_r, cl))
            new_adj <- c(new_adj, -1)
          }
        }
      }
      if(pipe_0 %in% c("|", "J", "7") && +1 %in% adj){
        if(pipe_p1 %in% c("|", "L", "F")){
          if(pipe_mark[rw, cl + 1] == "B"){
            new_idx <- c(new_idx, find_idx(rw + mv_r, cl))
            new_adj <- c(new_adj, +1)
          }
        }
      }
    }
    if(mv_c != 0){
      pipe_m1 <- pull_pipe(rw - 1, cl)
      pipe_p1 <- pull_pipe(rw + 1, cl)
      if(pipe_0 %in% c("-", "7", "F") && -1 %in% adj){
        if(pipe_m1 %in% c("-", "J", "L")){
          if(pipe_mark[rw - 1, cl] == "B"){
            new_idx <- c(new_idx, find_idx(rw, cl + mv_c))
            new_adj <- c(new_adj, -1)
          }
        }
      }
      if(pipe_0 %in% c("-", "J", "L") && +1 %in% adj){
        if(pipe_p1 %in% c("-", "7", "F")){
          if(pipe_mark[rw + 1, cl] == "B"){
            new_idx <- c(new_idx, find_idx(rw, cl + mv_c))
            new_adj <- c(new_adj, +1)
          }
        }
      }
    }

    if(length(new_idx) == 0){
      return(idx)
    }

    unlist(Map(adjust_idx, new_idx, mv_r, mv_c, new_adj))
  }

  # first fix the "B"s and not "B"s
  loop_mark <- find_pipe_loop(pipes)$pipes

  # indicate the number of columns and rows of the pipe
  n_c       <- ncol(pipes)
  n_r       <- nrow(pipes)

  # mark the indices, the row number, column number
  all_idx   <- seq_along(pipes)
  row_idx   <- c(row(pipes))
  col_idx   <- c(col(pipes))
  cmb_idx   <- combine_idx(row_idx, col_idx)


  # indicate the starting location and where the border is
  next_idx   <- all_idx[row_idx %in% c(1, n_r) | col_idx %in% c(1, n_c)]
  border_idx <- which(loop_mark == "B")

  # repeating the iterations
  repeat{
    outer_idx <- which(loop_mark == "O")
    next_idx  <- setdiff(x = next_idx,
                         y = c(outer_idx, border_idx))

    # if we have nothing left, break!
    if(length(next_idx) == 0){
      break;
    }

    # otherwise set the new locations to outer
    loop_mark[next_idx] <- "O"

    # update the flooding locations
    next_idx  <- sapply(X   = next_idx,
                        FUN = find_next_idx) |>
                 unlist()
  }

  return(loop_mark)
}

move_grid <- expand.grid(r = -1:1, c = -1:1) |>
             subset(r != 0 | c != 0)

print_loop <- function(x){
  x <- paste(apply(x, 1, paste, collapse = ""), collapse = "\n")
  sink(file = "~/Desktop/zzz.txt")

}


# 3. Part 1 ====================================================================

# turning the input into a matrix of pipes by splitting each row
pipes <- do.call(what = rbind,
                 args = strsplit(inp, ""))

# find the pipe loop
pipe_loop <- find_pipe_loop(pipes)
pipe_loop$count / 2

# 4. Part 2 ====================================================================

pipe_mark <- flood_pipes(pipes)

sum(pipe_mark != "O" & pipe_mark != "B")
