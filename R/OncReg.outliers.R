utils::globalVariables(".OR_nMAD_mat")
.OR.nMAD_mat <- matrix(NA, nrow = 100, ncol = 2)
.OR.nMAD_mat[3:100, 1] <- c(
  10.5, 4.72, 5.41, 4.19, 4.36, 3.86, 3.93, 3.66, 3.69, 3.52, 3.55, 3.43, 3.45,
  3.36, 3.37, 3.31, 3.32, 3.27, 3.27, 3.23, 3.24, 3.2, 3.21, 3.18, 3.18, 3.16,
  3.16, 3.14, 3.14, 3.13, 3.13, 3.11, 3.12, 3.1, 3.1, 3.09, 3.09, 3.08, 3.08,
  3.07, 3.07, 3.07, 3.07, 3.06, 3.06, 3.05, 3.05, 3.05, 3.05, 3.04, 3.04, 3.04,
  3.04, 3.03, 3.03, 3.03, 3.03, 3.02, 3.02, 3.02, 3.02, 3.01, 3.02, 3.01, 3.01,
  3.01, 3.01, 3.01, 3.01, 3, 3, 3, 3, 3, 3, 2.99, 3, 2.99, 2.99, 2.99, 2.99,
  2.99, 2.99, 2.99, 2.99, 2.98, 2.98, 2.98, 2.98, 2.98, 2.98, 2.98, 2.98, 2.98,
  2.98, 2.98, 2.98, 2.98
)
.OR.nMAD_mat[3:100, 2] <- c(
  21.54, 7.02, 8.05, 5.66, 5.89, 4.98, 5.07, 4.6, 4.65, 4.36, 4.39, 4.2, 4.21,
  4.08, 4.09, 3.98, 3.99, 3.91, 3.92, 3.85, 3.86, 3.81, 3.81, 3.77, 3.77, 3.73,
  3.74, 3.7, 3.71, 3.68, 3.68, 3.66, 3.66, 3.64, 3.64, 3.62, 3.62, 3.6, 3.61,
  3.59, 3.59, 3.58, 3.58, 3.57, 3.57, 3.56, 3.56, 3.55, 3.55, 3.54, 3.54, 3.53,
  3.53, 3.52, 3.52, 3.51, 3.51, 3.51, 3.51, 3.5, 3.5, 3.5, 3.5, 3.49, 3.49,
  3.49, 3.49, 3.48, 3.48, 3.48, 3.48, 3.47, 3.47, 3.47, 3.47, 3.46, 3.46, 3.46,
  3.46, 3.46, 3.46, 3.45, 3.45, 3.45, 3.45, 3.45, 3.45, 3.45, 3.45, 3.44, 3.44,
  3.44, 3.44, 3.44, 3.44, 3.44, 3.44, 3.43
)

#' Small-sample MAD cutoff under normality
#'
#' Returns the number of unscaled median absolute deviations from the median
#' required to exclude a proportion `p` of observations under normality, used in
#' the modified Z-score method for non-parametric outlier detection¹. Values
#' were pre-calculated via Monte Carlo root-finding using 10 million replicates
#' per iteration and rounded to two decimal places.
#' @param n Integer sample size, where `3` ≤ `n` ≤ `100`.
#' @param p Target two-sided exclusion probability. Only `0.05` and `0.025` are
#' currently implemented.
#' @return The cutoff in unscaled MAD units.
#' @references
#' 1. Iglewicz, B. and Hoaglin, D.C., 1993. Chapter 3.3 Modified Z-scores.
#' In: *How to Detect and Handle Outliers*, pp. 11–13. Milwaukee: ASQC Quality
#' Press.
#' @export
OR.nMAD <- function(n, p = 0.05) {
  if (p == 0.05) {
    col <- 1
  } else if (p == 0.025) {
    col <- 2
  } else {
    stop(paste0("[OR.nMAD] not implemented: p =", n))
  }
  if (n < 3 || n > nrow(.OR.nMAD_mat))
    stop(paste0("[OR.nMAD] not implemented: n =", n))
  return(.OR.nMAD_mat[n, col])
}

#' Single sample outlier detection using the modified Z-score method
#'
#' Flags observations whose absolute deviation from the sample median
#' exceeds a sample-size–adjusted multiple of the median absolute deviation.
#' @param x Numeric vector of observations.
#' @param p Target two-sided exclusion proportion under normality. That is, the
#' proportion of observations expected to lie beyond the cutoff in a sample of
#' size `n` from a normal distribution. Default = `0.05`.
#' @return Logical vector of the same length as `x` where `TRUE` indicates an
#' outlier.
#' @export
OR.outliers <- function(x, p = 0.05) {
  med <- stats::median(x)
  mad <- stats::mad(x)
  return(abs(x - med) > OR.nMAD(n = length(x), p = p) * mad)
}

# OR.outliers.rlm <- function(y, x, order = 3, p = 0.05) {
#
# }
