plot_Queens <- function(Queens){
  size <- nrow(Queens)
  board <- matrix(0, nrow = size, ncol = size)
  board[Queens] <- 1
  return(board)
}

plot_Q2 <- function(Queens){
  for(i in 1:nrow(Queens)){
    board <- board + get_Queen_infl(Queens[i,])
  }
  return(board)
}

check_Queens <- function(q1, q2){
  out <- TRUE
  if (q1[1] == q2[1] | q1[2] == q2[2]) {
    out <- FALSE
  } else if (q1[1] + q1[2] == q2[1] + q2[2]){
    out <- FALSE
  } else if (q1[1] - q1[2] == q2[1] - q2[2]){
    out <- FALSE
  }
  
  return(out)
}

check_Queens_all <- function(Queens){
  id_combn <- combn(nrow(Queens),2)
  out <- TRUE
  i <- 1
  while (out & i <= ncol(id_combn)){
    out <- out & check_Queens(Queens[id_combn[1,i],],
                              Queens[id_combn[2,i],])
    i <- i + 1
  }
  
  return(out)
}

get_Queen_infl <- function(q, q_itself = 0){
  size <- nrow(Queens)
  board <- matrix(0, nrow = size, ncol = size)
  board[q[1],] <- board[,q[2]] <- 1
  board[as.matrix(board_info %>% filter(sum == q[1]+q[2]) %>% 
                    select(row,col))] <- 1
  board[as.matrix(board_info %>% filter(dif == q[1]-q[2]) %>% 
                    select(row,col))] <- 1
  board[q[1],q[2]] <- q_itself
  return(board)
}

random_Queens <- function(){
  out <- cbind(sample(1:8,8,replace = FALSE),
               sample(1:8,8,replace = FALSE))
  return(out)
}

more_Queens <- function(Queens){
  out <- cbind(Queens, Queens[,1] + Queens[,2], Queens[,1] - Queens[,2])
  colnames(out) <- c("row", "col", "sum", "diff")
  return(out)
}