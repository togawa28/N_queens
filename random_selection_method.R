library(dplyr)

random_selection_method <- function(M, size = 8){
  make_init_BI <- function(){
    board_info <- data.frame(matrix(0, nrow = size*size, ncol = 4))
    colnames(board_info) <- c("row","col", "sum", "diff")
    board_info[,1] <- rep(1:size,  each = size)
    board_info[,2] <- rep(1:size, times = size)
    board_info[,3] <- board_info[,1] + board_info[,2]
    board_info[,4] <- board_info[,1] - board_info[,2]
    return(board_info)
  }
  
  RS_one_try <- function(){
    
    Queens <- matrix(NA, nrow = size, ncol = 2)
    Queens[,1] <- 1:size
    BI_updated <- make_init_BI()
    break_ind <- 0
    
    for(i in 1:size){
      cols_cands <- BI_updated %>% filter(row == i) %>% select(col)
      cols_cands <- as.matrix(cols_cands)
      if(nrow(cols_cands) == 0){
        break_ind <- 1
        break
      }else{
        if(nrow(cols_cands) == 1){
          qi_col <- as.integer(cols_cands)
        }else{
          qi_col <- sample(cols_cands, 1)
        }
        Queens[i, 2] <- qi_col
        qi_sum  <- i + qi_col
        qi_diff <- i - qi_col

        BI_updated <- BI_updated %>% filter(row != i,
                                            col != qi_col,
                                            sum != qi_sum,
                                            diff != qi_diff)
      }
    }
    
    return(list(Queens = Queens,
                break_ind = break_ind))
    
  }
  
  fetch_Qs <- function(QList){
    no_break_id <- which(unlist(lapply(QList, function(x) return(x$break_ind)))
                         == 0)
    out <- list()
    length(out) <- length(no_break_id)
    cnt <- 1
    for (i in no_break_id) {
      out[[cnt]] <- QList[[i]]$Queens
      cnt <- cnt + 1
    }
    return(out)
  }
  
  QList <- list()
  length(QList) <- M
  for ( m in 1:M ) {
    QList[[m]] <- RS_one_try()
  }
  
  return(fetch_Qs(QList))
}