#' Entropy balancing weights for the average treatment effect
#'
#' Computes entropy balancing¹ weights such that the weighted means of each
#' column of `X` in every arm equal those of the full sample.
#' @param X numeric covariate matrix.
#' @param arm vector of arm assignments, one per row of `X`.
#' @return A numeric vector of weights, summing to the arm size within each arm.
#' @references
#' 1. Hainmueller, J., 2012. Entropy balancing for causal effects: a
#' multivariate reweighting method to produce balanced samples in observational
#' studies. *Political Analysis*, 20(1), pp. 25–46.
#' @family ebal
#' @export
OR.ebal.ATE <- function(X, arm) {
  OR.ebal.target(X, arm, colMeans(X)) # target is the full-sample mean of each column
}

#' Entropy balancing weights to external target means
#'
#' Computes entropy balancing¹ weights such that the weighted means of each
#' column of `X` in every arm equal the external target means in `target`.
#' @param X numeric covariate matrix.
#' @param arm vector of arm assignments, one per row of `X`.
#' @param target numeric vector of target means, one per column of `X`.
#' @return A numeric vector of weights, summing to the arm size within each arm.
#' @references
#' 1. Hainmueller, J., 2012. Entropy balancing for causal effects: a
#' multivariate reweighting method to produce balanced samples in observational
#' studies. *Political Analysis*, 20(1), pp. 25–46.
#' @family ebal
#' @export
OR.ebal.target <- function(X, arm, target) {
  if (!is.matrix(X) || !is.numeric(X)) stop("[OR.ebal.target] X must be a numeric matrix")
  if (length(arm) != nrow(X)) stop("[OR.ebal.target] arm must have one element per row of X")
  if (length(target) != ncol(X)) stop("[OR.ebal.target] target must have one element per column of X")
  if (sum(is.na(X)) > 0) stop("[OR.ebal.target] X must not contain missing values")
  if (sum(is.na(arm)) > 0) stop("[OR.ebal.target] arm must not contain missing values")
  if (!is.numeric(target) || sum(!is.finite(target)) > 0) stop("[OR.ebal.target] target must be finite and numeric")
  w <- rep(NA_real_, nrow(X))
  for (a in unique(arm)) {
    is.arm <- arm == a
    w[is.arm] <- ebal::ebalance( # current arm reweighted to a single-row target
      Treatment = c(1, rep(0, sum(is.arm))),
      X = rbind(target, X[is.arm, , drop = FALSE]),
      constraint.tolerance = 1e-8,
      print.level = -1)$w * # suppress convergence message
      sum(is.arm) # rescale to sum to arm size
  }
  return(w)
}
