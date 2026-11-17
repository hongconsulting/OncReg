#' Last observation carried forward
#'
#' Applies LOCF imputation to `x` within each `id`, assuming observations are
#' already ordered within each `id`.
#' @param x Vector of values for LOCF imputation.
#' @param id Optional vector of cluster identifiers. If `NULL`, all observations
#' are treated as a single cluster.
#' @return Vector of same length as `x` with LOCF applied within each `id`.
#' @family imputation
#' @export
OR.LOCF <- function(x, id = NULL) {
  if (is.null(id)) id <- rep(0, length(x))
  output <- rep(NA, length(x))
  unique_id <- unique(id)
  for (i in seq_along(unique_id)) {
    current_indices <- which(id == unique_id[i])
    current_x <- x[current_indices]
    last <- NA
    for (j in seq_along(current_x)) {
      if (!is.na(current_x[j])) {
        last <- current_x[j]
      } else {
        current_x[j] <- last
      }
    }
    output[current_indices] <- current_x
  }
  return(output)
}

#' Rubin's rules
#'
#' Pools estimates and variances across multiply imputed datasets using Rubin's
#' rules¹, with small-sample correction² for *t*-based complete-data analyses.
#' @param beta Numeric vector of point estimates, one per imputed dataset.
#' @param SE Numeric vector of standard errors, one per imputed dataset.
#' @param df.com Complete-data degrees of freedom, e.g. `df.residual()` of a
#' linear model; `Inf` for *z*-based models such as Cox. Default = `Inf`.
#' @return A named numeric vector with the pooled estimate, standard error, 95%
#' confidence limits, and two-sided *p*-value.
#' @references
#' 1. Rubin, D.B., 1987. *Multiple Imputation for Nonresponse in Surveys*. New
#' York: Wiley.
#' 2. Barnard, J. and Rubin, D.B., 1999. Small-sample degrees of freedom with
#' multiple imputation. *Biometrika*, 86(4), pp. 948–955.
#' @family imputation
#' @export
OR.Rubin <- function(beta, SE, df.com = Inf) {
  # Range checks
  m <- length(beta)
  if (length(SE) != m) stop("[OR.Rubin] beta and SE must have same length")
  if (m < 2) stop("[OR.Rubin] at least 2 imputed datasets required")
  if (sum(!is.finite(beta)) > 0) stop("[OR.Rubin] beta must be finite")
  if (sum(!is.finite(SE)) > 0) stop("[OR.Rubin] SE must be finite")
  if (min(SE) <= 0) stop("[OR.Rubin] SE must be positive")
  if (length(df.com) != 1 || is.na(df.com) || df.com <= 0) stop("[OR.Rubin] df.com must be a single positive number")
  beta.pooled <- mean(beta)
  var.within <- mean(SE^2)
  var.between <- stats::var(beta)
  var.total <- var.within + (1 + 1 / m) * var.between
  lambda <- (1 + 1 / m) * var.between / var.total
  df <- (m - 1) / lambda^2
  if (is.finite(df.com)) {
    df <- 1 / (1 / df + 1 / ((df.com + 1) / (df.com + 3) * df.com * (1 - lambda)))
  }
  SE.pooled <- sqrt(var.total)
  t.crit <- stats::qt(0.975, df)
  return(c(beta = beta.pooled,
           SE = SE.pooled,
           lower = beta.pooled - t.crit * SE.pooled,
           upper = beta.pooled + t.crit * SE.pooled,
           p = 2 * stats::pt(-abs(beta.pooled / SE.pooled), df)))
}
