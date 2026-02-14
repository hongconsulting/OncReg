utils::globalVariables(".OR_nMAD_mat")
.OR.nMAD_mat <- matrix(NA, nrow = 20, ncol = 1)
.OR.nMAD_mat[3:20, 1] <- c(
  10.51, 4.72, 5.41, 4.19, 4.36, 3.86, 3.93,
  3.66, 3.69, 3.53, 3.55, 3.43, 3.45, 3.36,
  3.37, 3.31, 3.32, 3.27
)

#' Small-sample MAD cutoff under normality
#'
#' Returns the number of unscaled median absolute deviations from the median
#' required to exclude a proportion `p` of observations under normality, used in
#' the modified Z-score method for non-parametric outlier detection¹. Values
#' were obtained via Monte Carlo root-finding using 10 million replicates per
#' iteration and rounded to two decimal places.
#' @param n Integer sample size, where `3` ≤ `n` ≤ `20`.
#' @param p Target two-sided exclusion probability. Only `0.05` is currently
#' implemented.
#' @return Numeric scalar giving the cutoff in unscaled MAD units.
#' @references
#' 1. Iglewicz, B. and Hoaglin, D.C., 1993. Chapter 3.3 Modified Z-scores.
#' In: *How to Detect and Handle Outliers*, pp. 11–13. Milwaukee: ASQC Quality
#' Press.
#' @export
OR.nMAD <- function(n, p = 0.05) {
  if (p == 0.05) {
    col <- 1
  } else {
    stop(paste0("[OR.nMAD] not implemented: p =", n))
  }
  if (n < 3 || n > nrow(.OR_nMAD_mat))
    stop(paste0("[OR.nMAD] not implemented: n =", n))
  return(.OR_nMAD_mat[n, col])
}
