library(dplyr)
source("random_selection_method.R")
source("ex_functions.R")

#### case 1: size = 8, M = 100 ####
set.seed(123)
M <- 100
system.time(Qs <- random_selection_method(M, size = 8))
# ユーザ   システム       経過  
# 1.543      0.017      1.575 
length(Qs)
# 14

# test output
# first element
Qs[[1]]
plot_Queens(Qs[[1]])


#### case 2: size = 10, M = 100 ####
set.seed(123)
system.time(Qs <- random_selection_method(M, size = 10))
# ユーザ   システム       経過  
# 1.974      0.021      2.017 
length(Qs)
# [1] 8

#### case 3: size = 30, M = 500 ####
set.seed(123)
M <- 500
system.time(Qs <- random_selection_method(M, size = 30))
# ユーザ   システム       経過  
# 27.494      0.109     27.685 
length(Qs)
# [1] 5

#### Improvements? #####
# 1) spending too much time;
# perhaps we could weight the probabilities of selecting the col from col_cands?
# needs some weight function to make the selection step more efficient
# 2) finding not too many patterns;
# found only 5 patterns when size = 30 (acceptance rate is just 1%)
# maybe similar solution as in 1), utilizing some weight function?
# needs to take a close look at the outputs: 
# some of them should be eccentially same e.g. identical by rotation

