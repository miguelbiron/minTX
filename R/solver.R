#' Minimize number of payments using linear programming
#'
#' @param D matrix of payments between members of size \eqn{n \times n} with
#' nonnegative entries. Diagonal entries must be equal to zero
#' @param b vector of size \eqn{n} with net income of each member (positive or
#' negative). Entries of \code{b} must add up to 0. If both \code{b} and \code{D} are
#' present, \code{b} is used
#'
#' @return Matrix of payments of size \eqn{n \times n} with the least number
#' of nonzero entries
#'
#' @examples
#' D = matrix(rexp(5*5, 1/10), nrow = 5); diag(D) = rep(0, 5)
#' solve_min_TX(D)
#'
#' @export
solve_min_TX = function(D = NULL, b = NULL){

  if(is.null(b)){
    stopifnot(min(D) >= 0)
    stopifnot(max(diag(D)) <= 1e-9)
    b = colSums(D) - rowSums(D)
  }

  stopifnot(sum(b) <= 1e-9)
  n = length(b)
  k = n*(n-1)

  # solve lp
  fit_lp = lpSolve::lp(
    objective.in = rep(1, k),
    const.mat    = get_const_matrix(n),
    const.dir    = rep("==", n),
    const.rhs    = b
  )

  print(fit_lp)

  return(vec_2_mat(fit_lp$solution))
}
