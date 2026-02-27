utils::globalVariables(".OR_nMAD_mat")
.OR.nMAD_mat <- matrix(NA, nrow = 100, ncol = 4)
.OR.nMAD_mat[3:100, 1] <- c(
  10.5, 4.72, 5.4,  4.19, 4.36, 3.86, 3.93, 3.66, 3.69, 3.52, 3.55, 3.43, 3.45, 
  3.36, 3.37, 3.31, 3.32, 3.27, 3.27, 3.23, 3.24, 3.2,  3.21, 3.18, 3.18, 3.16, 
  3.16, 3.14, 3.14, 3.13, 3.13, 3.11, 3.12, 3.1,  3.1,  3.09, 3.09, 3.08, 3.08, 
  3.07, 3.07, 3.07, 3.07, 3.06, 3.06, 3.05, 3.05, 3.05, 3.05, 3.04, 3.04, 3.04, 
  3.04, 3.03, 3.03, 3.03, 3.03, 3.02, 3.02, 3.02, 3.02, 3.01, 3.02, 3.01, 3.01, 
  3.01, 3.01, 3.01, 3.01, 3,    3,    3,    3,    3,    3,    2.99,    3, 2.99, 
  2.99, 2.99, 2.99, 2.99, 2.99, 2.99, 2.99, 2.98, 2.98, 2.98, 2.98, 2.98, 2.98, 
  2.98, 2.98, 2.98, 2.98, 2.98, 2.98, 2.97
)
.OR.nMAD_mat[3:100, 2] <- c(
  21.58,7.03, 8.05, 5.66, 5.89, 4.98, 5.07, 4.6,  4.65, 4.36, 4.39, 4.2,  4.21, 
  4.07, 4.09, 3.98, 3.99, 3.91, 3.92, 3.85, 3.86, 3.81, 3.81, 3.77, 3.77, 3.73, 
  3.74, 3.7,  3.71, 3.68, 3.68, 3.66, 3.66, 3.64, 3.64, 3.62, 3.62, 3.6,  3.61, 
  3.59, 3.59, 3.58, 3.58, 3.57, 3.57, 3.56, 3.56, 3.55, 3.55, 3.54, 3.54, 3.53, 
  3.53, 3.52, 3.52, 3.51, 3.52, 3.51, 3.51, 3.5,  3.5,  3.5,  3.5,  3.49, 3.49, 
  3.49, 3.49, 3.48, 3.48, 3.48, 3.48, 3.47, 3.47, 3.47, 3.47, 3.46, 3.46, 3.46, 
  3.46, 3.46, 3.46, 3.45, 3.45, 3.45, 3.45, 3.45, 3.45, 3.44, 3.45, 3.44, 3.44, 
  3.44, 3.44, 3.44, 3.44, 3.43, 3.44, 3.43
)
.OR.nMAD_mat[3:100, 3] <- c(
  54.62,11.54,13.24,8.14, 8.49, 6.75, 6.87, 6.01, 6.07, 5.57, 5.6,  5.27, 5.29, 
  5.06, 5.08, 4.9,  4.91, 4.78, 4.79, 4.68, 4.69, 4.6,  4.61, 4.54, 4.54, 4.48, 
  4.48, 4.43, 4.43, 4.39, 4.39, 4.35, 4.36, 4.32, 4.32, 4.29, 4.3,  4.27, 4.27, 
  4.25, 4.25, 4.22, 4.23, 4.21, 4.21, 4.19, 4.19, 4.17, 4.17, 4.16, 4.16, 4.15, 
  4.15, 4.13, 4.13, 4.12, 4.12, 4.11, 4.11, 4.1,  4.1,  4.09, 4.09, 4.08, 4.08, 
  4.08, 4.08, 4.07, 4.07, 4.06, 4.06, 4.05, 4.05, 4.05, 4.05, 4.04, 4.04, 4.04, 
  4.04, 4.03, 4.03, 4.03, 4.03, 4.02, 4.02, 4.02, 4.02, 4.01, 4.01, 4.01, 4.01, 
  4,    4,    4,    4,    4,    4,    3.9
)
.OR.nMAD_mat[3:100, 4] <- c(
  109.71,16.58,19.1,10.56,11.02,8.34, 8.5,  7.24, 7.31, 6.58, 6.62, 6.15, 6.18, 
  5.86, 5.87, 5.63, 5.64, 5.46, 5.47, 5.32, 5.33, 5.21, 5.22, 5.12, 5.13, 5.04, 
  5.05, 4.98, 4.98, 4.92, 4.93, 4.87, 4.88, 4.83, 4.83, 4.79, 4.79, 4.76, 4.76, 
  4.73, 4.73, 4.7,  4.7,  4.68, 4.68, 4.65, 4.65, 4.63, 4.63, 4.61, 4.61, 4.59, 
  4.59, 4.58, 4.58, 4.56, 4.56, 4.55, 4.55, 4.54, 4.54, 4.52, 4.52, 4.51, 4.51, 
  4.5,  4.5,  4.49, 4.49, 4.48, 4.48, 4.47, 4.47, 4.46, 4.46, 4.46, 4.46, 4.45, 
  4.45, 4.44, 4.44, 4.43, 4.43, 4.43, 4.43, 4.42, 4.42, 4.41, 4.42, 4.41, 4.41, 
  4.4,  4.4,  4.4,  4.4,  4.39, 4.39, 4.39
)

#' Small-sample MAD cutoff under normality
#'
#' Returns the number of unscaled median absolute deviations from the median
#' required to exclude a proportion `p` of observations under normality, used in
#' the modified Z-score method for non-parametric outlier detection¹. Values
#' were pre-calculated via Monte Carlo root-finding using 10 million replicates
#' per iteration and rounded to two decimal places.
#' @param n Integer sample size, where `3` ≤ `n` ≤ `100`.
#' @param p Target two-sided exclusion probability. Only `0.05`, `0.025`, `0.01`,
#' and `0.005` are currently implemented.
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
    stop(paste0("[OR.nMAD] not implemented: p = ", p))
  }
  if (n < 3 || n > nrow(.OR.nMAD_mat))
    stop(paste0("[OR.nMAD] not implemented: n = ", n))
  return(.OR.nMAD_mat[n, col])
}

#' Single sample non-parametric outlier detection using the modified Z-score method
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
  MAD <- stats::mad(x)
  return(abs(x - med) > OR.nMAD(n = length(x), p = p) * MAD)
}

#' Polynomial linear non-parametric outlier detection
#'
#' Fits a univariable polynomial regression using Huber M-estimation with
#' degree selected by robust AIC (AICR), then flags observations whose
#' residuals exceed a small-sample–adjusted multiple of the median absolute
#' deviation.
#' @param x Numeric predictor vector.
#' @param y Numeric response vector.
#' @param maxdegree Maximum polynomial degree considered. Default = `3`.
#' @param p Target two-sided exclusion proportion under normality for the
#' residual-based modified Z-score rule. Default = `0.05`.
#' @return Logical vector of the same length as `y` where `TRUE` indicates an
#' outlying observation relative to the AICR-selected robust polynomial fit.
#' @export
OR.outliers.rlm <- function(x, y, maxdegree = 3, p = 0.05) {
  fit <- rlm.Huber.univarpoly.AICR(x = x, y = y, maxdegree = maxdegree, p = p)
  return(OR.outliers(x = fit$resid, p = p))
}

OR.outliers.rlm.plot <- function(x, y, maxdegree = 3, p = 0.05,
                                 x.breaks = NA, x.labels = NA,
                                 y.min = NA, y.max = NA) {
  n <- length(y)
  fit <- rlm.Huber.univarpoly.AICR(x = x, y = y, maxdegree = maxdegree, p = p)
  fitted <- fit$fitted
  resid <- fit$resid
  MAD <- stats::mad(resid)
  upper <- fitted + OR.nMAD(n, p) * MAD
  lower <- fitted - OR.nMAD(n, p) * MAD
  out <- OR.outliers(x = resid, p = p)
  df <- data.frame("x" = x, "y" = y, "fitted" = fitted,
                   "lower" = lower, "upper" = upper, "out" = out)
  ymin <- min(c(df$y, df$lower, y.min), na.rm = T)
  ymax <- max(c(df$y, df$upper, y.max), na.rm = T)

  if (length(x.breaks) == 1) if (is.na(x.breaks)) x.breaks <- x
  if (length(x.labels) == 1) if (is.na(x.labels)) x.labels <- x
  g <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = "blue", alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = fitted),
      colour = "blue"
    ) +
    ggplot2::geom_point(
      data = df[!df$out, ],
      ggplot2::aes(y = y),
      colour = "blue"
    ) +
    ggplot2::geom_point(
      data = df[df$out, ],
      ggplot2::aes(y = y),
      colour = "red"
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_continuous(breaks = x.breaks, labels = x.labels) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                              limits = c(floor(ymin), ceiling(ymax)))
  print(df)
  return(g)
}

# resid <- fit$resid
# fitted <- fit$fitted
# MAD <- stats::mad(resid)
# upper <- fitted +
#   out <- OR.outliers(x = resid, p = p)
