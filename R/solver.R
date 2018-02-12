solve_min_TX = function(X){
  k = nrow(X)*(nrow(X)-1)

  # solve lp
  fit_lp = lpSolve::lp(
    objective.in = rep(1, k),
    const.mat    = get_const_matrix(n),
    const.dir    = rep("==", n),
    const.rhs    = colSums(X) - rowSums(X)
  )

  print(fit_lp)

  return(vec_2_mat(fit_lp$solution))
}
