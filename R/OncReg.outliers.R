utils::globalVariables(".OR.kMAD.mat")
.OR.kMAD.mat <- matrix(NA, nrow = 100, ncol = 4)
.OR.kMAD.mat[3:100, 1] <- c(
  10.5, 4.72, 5.4,  4.19, 4.36, 3.86, 3.93, 3.66, 3.69, 3.52, 3.55, 3.43, 3.45,
  3.36, 3.37, 3.31, 3.32, 3.27, 3.27, 3.23, 3.24, 3.2,  3.21, 3.18, 3.18, 3.16,
  3.16, 3.14, 3.14, 3.13, 3.13, 3.11, 3.12, 3.1,  3.1,  3.09, 3.09, 3.08, 3.08,
  3.07, 3.07, 3.07, 3.07, 3.06, 3.06, 3.05, 3.05, 3.05, 3.05, 3.04, 3.04, 3.04,
  3.04, 3.03, 3.03, 3.03, 3.03, 3.02, 3.02, 3.02, 3.02, 3.01, 3.02, 3.01, 3.01,
  3.01, 3.01, 3.01, 3.01, 3,    3,    3,    3,    3,    3,    2.99,    3, 2.99,
  2.99, 2.99, 2.99, 2.99, 2.99, 2.99, 2.99, 2.98, 2.98, 2.98, 2.98, 2.98, 2.98,
  2.98, 2.98, 2.98, 2.98, 2.98, 2.98, 2.97
)
.OR.kMAD.mat[3:100, 2] <- c(
  21.58,7.03, 8.05, 5.66, 5.89, 4.98, 5.07, 4.6,  4.65, 4.36, 4.39, 4.2,  4.21,
  4.07, 4.09, 3.98, 3.99, 3.91, 3.92, 3.85, 3.86, 3.81, 3.81, 3.77, 3.77, 3.73,
  3.74, 3.7,  3.71, 3.68, 3.68, 3.66, 3.66, 3.64, 3.64, 3.62, 3.62, 3.6,  3.61,
  3.59, 3.59, 3.58, 3.58, 3.57, 3.57, 3.56, 3.56, 3.55, 3.55, 3.54, 3.54, 3.53,
  3.53, 3.52, 3.52, 3.51, 3.52, 3.51, 3.51, 3.5,  3.5,  3.5,  3.5,  3.49, 3.49,
  3.49, 3.49, 3.48, 3.48, 3.48, 3.48, 3.47, 3.47, 3.47, 3.47, 3.46, 3.46, 3.46,
  3.46, 3.46, 3.46, 3.45, 3.45, 3.45, 3.45, 3.45, 3.45, 3.44, 3.45, 3.44, 3.44,
  3.44, 3.44, 3.44, 3.44, 3.43, 3.44, 3.43
)
.OR.kMAD.mat[3:100, 3] <- c(
  54.62,11.54,13.24,8.14, 8.49, 6.75, 6.87, 6.01, 6.07, 5.57, 5.6,  5.27, 5.29,
  5.06, 5.08, 4.9,  4.91, 4.78, 4.79, 4.68, 4.69, 4.6,  4.61, 4.54, 4.54, 4.48,
  4.48, 4.43, 4.43, 4.39, 4.39, 4.35, 4.36, 4.32, 4.32, 4.29, 4.3,  4.27, 4.27,
  4.25, 4.25, 4.22, 4.23, 4.21, 4.21, 4.19, 4.19, 4.17, 4.17, 4.16, 4.16, 4.15,
  4.15, 4.13, 4.13, 4.12, 4.12, 4.11, 4.11, 4.1,  4.1,  4.09, 4.09, 4.08, 4.08,
  4.08, 4.08, 4.07, 4.07, 4.06, 4.06, 4.05, 4.05, 4.05, 4.05, 4.04, 4.04, 4.04,
  4.04, 4.03, 4.03, 4.03, 4.03, 4.02, 4.02, 4.02, 4.02, 4.01, 4.01, 4.01, 4.01,
  4,    4,    4,    4,    4,    4,    3.9
)
.OR.kMAD.mat[3:100, 4] <- c(
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
#' the modified *z*-score method for non-parametric outlier detection¹. Values
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
OR.kMAD <- function(n, p = 0.05) {
  if (p == 0.05) {
    col <- 1
  } else if (p == 0.025) {
    col <- 2
  } else if (p == 0.01) {
    col <- 3
  } else if (p == 0.005) {
    col <- 4
  } else {
    stop(paste0("[OR.kMAD] not implemented: p = ", p))
  }
  if (n < 3 || n > nrow(.OR.kMAD.mat))
    stop(paste0("[OR.kMAD] not implemented: n = ", n))
  return(.OR.kMAD.mat[n, col])
}

#' Non-parametric outlier detection using the modified *z*-score method
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
  return(abs(x - med) > OR.kMAD(n = length(x), p = p) * MAD)
}

#' Polynomial linear non-parametric outlier detection
#'
#' Fits a univariable polynomial regression using Huber M-estimation with
#' degree selected by robust AIC (AICR), then flags observations whose
#' residuals exceed a small-sample–adjusted multiple of the median absolute
#' deviation.
#' @param x Numeric predictor vector.
#' @param y Numeric response vector.
#' @param max.degree Maximum polynomial degree considered. Default = `3`.
#' @param p Target two-sided exclusion proportion under normality for the
#' residual-based modified *z*-score rule. Default = `0.05`.
#' @return Logical vector of the same length as `y` where `TRUE` indicates an
#' outlying observation relative to the AICR-selected robust polynomial fit.
#' @details
#' For numerical stability, `x` and `y` are standardized before fitting.
#' @export
OR.outliers.rlm <- function(x, y, max.degree = 3, p = 0.05) {
  mx <- mean(x, na.rm = TRUE)
  sx <- stats::sd(x, na.rm = TRUE)
  my <- mean(y, na.rm = TRUE)
  sy <- stats::sd(y, na.rm = TRUE)
  x_z <- (x - mx) / sx
  y_z <- (y - my) / sy
  fit <- rlm.Huber.univarpoly.AICR(x = x_z, y = y_z, max.degree = max.degree, p = p)
  return(OR.outliers(x = fit$resid, p = p))
}

#' Polynomial linear non-parametric outlier plot
#'
#' Fits a univariable polynomial regression using Huber M-estimation with
#' degree selected by robust AIC (AICR), then flags observations whose
#' residuals exceed a small-sample–adjusted multiple (*k*) of the median absolute
#' deviation (MAD).
#' @param x Numeric predictor vector.
#' @param y Numeric response vector.
#' @param max.degree Maximum polynomial degree considered. Default = `3`.
#' @param p Target two-sided exclusion proportion under normality for the
#' residual-based modified *z*-score rule. Default = `0.05`.
#' @param col.in Colour used for the fitted curve, ribbon band, and observations
#' not flagged as outliers. Default = `"#0072B5FF"`.
#' @param col.out Colour used for observations flagged as outliers. Default =
#' `"#BC3C29FF"`.
#' @param echo Logical. If `TRUE`, prints the internal data frame used for the
#' plot together with the computed *k* and MAD values.
#' @param x.breaks Numeric vector specifying x-axis tick locations. If `NA`,
#' the values of `x` are used.
#' @param x.labels Labels for the x-axis ticks. If `NA`, the values of `x`
#' are used.
#' @param x.title Title for the x-axis.
#' @param y.breaks Numeric vector specifying y-axis tick locations. Horizontal
#' gridlines are drawn at these values.
#' @param y.title Title for the y-axis.
#' @details
#' For numerical stability, `x` and `y` are standardized before fitting and
#' back-transformed for plotting.
#' @examples
#' y <- c(36.3, 47.9, 47.2, 43.9, 47.6, 49.6, 53.2, 59.3, 63.2, 70.8, 75.9, 88.5,
#'        97.3, 103.6, 6.1, 120.2, 135.8, 139.4)
#' x <- 1:length(y) - 1
#' OR.outliers.rlm.ggplot(x, y, max.degree = 5, p = 0.01, x.title = "X",
#'                        y.breaks = seq(0, 150, 50), y.title = "Y")
#' @export
OR.outliers.rlm.ggplot <- function(x, y, max.degree = 3, p = 0.05,
                                 col.in =  "#0072B5FF", col.out = "#BC3C29FF", echo = FALSE,
                                 x.breaks = NA, x.labels = NA, x.title = "",
                                 y.breaks = NA, y.title = "") {
  n <- length(y)
  mx <- mean(x, na.rm = TRUE)
  sx <- stats::sd(x, na.rm = TRUE)
  my <- mean(y, na.rm = TRUE)
  sy <- stats::sd(y, na.rm = TRUE)
  x_z <- (x - mx) / sx
  y_z <- (y - my) / sy
  fit <- rlm.Huber.univarpoly.AICR(x = x_z, y = y_z, max.degree = max.degree, p = p)
  fitted <- my + sy * fit$fitted
  resid  <- sy * fit$resid
  MAD <- stats::mad(resid)
  upper <- fitted + OR.kMAD(n, p) * MAD
  lower <- fitted - OR.kMAD(n, p) * MAD
  out <- OR.outliers(x = resid, p = p)
  df <- data.frame("x" = x, "y" = y, "fitted" = fitted,
                   "lower" = lower, "upper" = upper, "out" = out)
  ymin <- min(c(df$y, df$lower, y.breaks), na.rm = T)
  ymax <- max(c(df$y, df$upper, y.breaks), na.rm = T)
  if (length(x.breaks) == 1) if (is.na(x.breaks)) x.breaks <- x
  if (length(x.labels) == 1) if (is.na(x.labels)) x.labels <- x
  g <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::geom_hline(yintercept = y.breaks, color = grDevices::rgb(0.95, 0.95, 0.95)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = col.in, alpha = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = fitted),
      colour = col.in
    ) +
    ggplot2::geom_point(
      data = df[!df$out, ],
      ggplot2::aes(y = y),
      colour = col.in
    ) +
    ggplot2::geom_point(
      data = df[df$out, ],
      ggplot2::aes(y = y),
      colour = col.out
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_continuous(breaks = x.breaks, labels = x.labels) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                limits = c(floor(ymin), ceiling(ymax))) +
    ggplot2::labs(
      x = x.title,
      y = y.title
    )
  if (echo) {
    print(df)
    cat("[OR.outliers.rlm.ggplot] k = ",  OR.kMAD(n, p), ", MAD = ", MAD, "\n", sep = "")
  }
  return(g)
}


