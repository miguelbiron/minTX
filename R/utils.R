###############################################################################
# utils
###############################################################################

get_M_matrix = function(n){
  # returns matrix of size n \times n(n-1)
  # input
  #    n: number of participants
  stopifnot(n>=2)
  
  block = rbind(rep(1L, n-1),
                -diag(n-1))
  b_list = vector(mode = "list", length = n)
  b_list[[1]] = block
  
  for(i in 2:n){
    block[(i-1):i, ] = block[i:(i-1), ]
    b_list[[i]] = block
  }
  
  return(do.call(cbind, b_list))
}

mat_2_vec = function(X){
  X[lower.tri(X) | upper.tri(X)]
}

vec_2_mat = function(x){
  n = trunc(0.5 * (1 + sqrt(1 + 4*length(x))))
  X = matrix(0, n, n)
  X[lower.tri(X) | upper.tri(X)] = x
  return(X)
}
