#' Random hold-out allocation
#'
#' Randomly allocates `n` observations to a hold-out set of size
#' `ceiling(n * proportion)`, with the remainder allocated to the training set.
#' The number of hold-out observations is fixed by construction rather than
#' binomially distributed and the output is reproducible for a given `seed`.
#' @param n Integer giving the number of observations to allocate.
#' @param proportion Numeric giving the proportion of observations allocated to
#' the hold-out set, rounded up to the nearest whole observation.
#' @param seed Integer seed for random number generation. Default = `24601`.
#' @return Numeric vector of length `n` containing `1` for hold-out observations
#' and `0` for training observations.
#' @family other
#' @export
OR.holdout <- function(n, proportion, seed = 24601) {
  set.seed(seed)
  k <- ceiling(n * proportion)
  return(sample(rep(c(1, 0), c(k, n - k))))
}

#' Enumerate permutations of k out of n items
#'
#' Returns all length-`k` permutations of the integers from 1 to `n`, in
#' lexicographic order.
#' @param n Integer size of the set.
#' @param k Integer number of elements per permutation.
#' @return Integer matrix with `factorial(n)/factorial(n-k)` rows and `k` columns.
#' @family other
#' @export
OR.permutations <- function(n, k) {
  if (k > n) stop("k > n")
  total <- factorial(n) / factorial(n - k)
  output <- matrix(0, total, k)
  a <- seq_len(n)
  output[1, ] <- a[1:k]
  i <- 1
  while (TRUE) {
    j <- n - 1
    while (j > 0 && a[j] > a[j + 1]) j <- j - 1
    if (j == 0) break
    l <- n
    while (a[j] > a[l]) l <- l - 1
    temp <- a[j]; a[j] <- a[l]; a[l] <- temp
    a[(j + 1):n] <- rev(a[(j + 1):n])
    i <- i + 1
    output[i, ] <- a[1:k]
  }
  return(output)
}

#' Restricted cubic spline basis
#'
#' Returns the restricted cubic spline basis of `x`. The first column is `x`
#' itself and the next `k - 2` columns are the nonlinear terms, divided by the
#' squared distance between the outer knots. If `x` has missing values, a final
#' column indicates missingness.
#' @param x Numeric vector.
#' @param k Number of knots, where `k` ≥ `3`. Ignored if `knots` is specified.
#' Default = `3`.
#' @param knots Optional numeric vector of knot locations. Default = `NULL`.
#' @param missing.ref Value of `x` at which the spline columns are evaluated for
#' missing observations, so that the missing indicator compares missing
#' observations with `x` = `missing.ref`. Required if `x` has missing values,
#' otherwise ignored. Default = `NULL`.
#' @param name Optional string prefixed to the column names. Default = `NULL`.
#' @return Numeric matrix with `length(x)` rows and one column fewer than the
#' number of knots (named `_1`, `_2`, ..., prefixed by `name`), plus a
#' missing-indicator column (named `_missing`, prefixed by `name`) if `x` has
#' missing values. Knot locations are returned in the `"knots"` attribute.
#' @details
#' Knot locations and the basis are the same as `rms::rcs()`: by default,
#' knots are placed at equally spaced quantiles of the non-missing values of
#' `x`, with adjustments for small samples and ties. Unlike `rms::rcs()`, which
#' may silently use fewer knots than requested, an error is thrown if the number
#' of distinct knots differs from the number requested.
#'
#' Missing values are handled differently; `rms::rcs()` returns `NA` rows, so
#' observations with missing `x` are dropped from a subsequent regression. Here,
#' missing observations are assigned the spline columns of `x` = `missing.ref`
#' and a column indicating missingness is appended, so that these observations
#' are retained and the indicator compares them with `x` = `missing.ref`.
#' @family other
#' @export
OR.RCS <- function(x, k = 3, knots = NULL, missing.ref = NULL, name = NULL) {
  k.requested <- if (is.null(knots)) k else length(knots)
  if (is.null(knots)) {
    x.obs <- x[!is.na(x)]
    n <- length(x.obs)
    if (k < 3) stop(paste0("[OR.RCS] not implemented: k = ", k))
    x.unique <- sort(unique(x.obs))
    if (length(x.unique) - 2 <= k) {
      # few unique values: use the interior unique values as knots
      knots <- x.unique[-c(1, length(x.unique))]
    } else {
      outer <- if (k == 3) 0.1 else if (k <= 6) 0.05 else 0.025
      # outer knots moved inside heavy ties at the minimum or maximum
      knot.first <- NULL
      knot.last <- NULL
      freq <- table(x.obs) / n
      if (max(freq[-c(1, length(freq))]) < 0.05) {
        if (freq[1] >= 0.05) {
          knot.first <- min(x.obs[x.obs > min(x.obs)])
          x.obs <- x.obs[x.obs > knot.first]
        }
        if (freq[length(freq)] >= 0.05) {
          knot.last <- max(x.obs[x.obs < max(x.obs)])
          x.obs <- x.obs[x.obs < knot.last]
        }
      }
      k.inner <- k - length(knot.first) - length(knot.last)
      if (k.inner == 1) {
        knots <- stats::median(x.obs)
      } else {
        p <- if (k.inner == 2) {
          seq(0.5, 1 - outer, length.out = 2)
        } else {
          seq(outer, 1 - outer, length.out = k.inner)
        }
        knots <- unname(stats::quantile(x.obs, p))
        # small samples: outer knots at the 5th smallest and 5th largest values
        if (length(x.obs) < 100) {
          x.obs <- sort(x.obs)
          if (is.null(knot.first)) knots[1] <- x.obs[5]
          if (is.null(knot.last)) knots[k.inner] <- x.obs[length(x.obs) - 4]
        }
      }
      knots <- c(knot.first, knots, knot.last)
    }
  }
  knots <- sort(unique(knots))
  k <- length(knots)
  if (k != k.requested)
    stop(paste0("[OR.RCS] ", k, " distinct knots obtained; ", k.requested,
                " requested"))
  if (k < 3) stop(paste0("[OR.RCS] not implemented: ", k, " knots"))
  # missing observations: evaluated at x = missing.ref
  is.missing <- is.na(x)
  if (sum(is.missing) > 0) {
    if (is.null(missing.ref))
      stop("[OR.RCS] x has missing values: supply missing.ref")
    x[is.missing] <- missing.ref
  }
  # scale for the nonlinear terms
  kd <- (knots[k] - knots[1])^(2/3)
  output <- matrix(x, nrow = length(x), ncol = k - 1)
  for (j in 1:(k - 2)) {
    output[, j + 1] <- pmax((x - knots[j]) / kd, 0)^3 +
      ((knots[k - 1] - knots[j]) * pmax((x - knots[k]) / kd, 0)^3 -
         (knots[k] - knots[j]) * pmax((x - knots[k - 1]) / kd, 0)^3) /
      (knots[k] - knots[k - 1])
  }
  colnames(output) <- paste0(name, "_", 1:(k - 1))
  # missing indicator
  if (sum(is.missing) > 0) {
    output <- cbind(output, as.numeric(is.missing))
    colnames(output)[k] <- paste0(name, "_missing")
  }
  attr(output, "knots") <- knots
  return(output)
}

#' Read CSV files matching a path stub and combine by rows
#'
#' Reads all comma-separated values (CSV) files whose paths begin with `path.stub`
#' and combines them by rows.
#' @param path.stub String path prefix used to match CSV files.
#' @param ... Additional arguments passed to `read.csv()`.
#' @return A `data.frame` containing contents of all matched CSV files combined
#' by rows.
#' @family other
#' @export
OR.read.csv.stub <- function(path.stub, ...) {
  files <- Sys.glob(paste0(path.stub, "*.csv"))
  output <- do.call(rbind, lapply(files, utils::read.csv, ...))
  return(output)
}

#' Standardize a continuous variable by age and sex
#'
#' Within each sex, fits a linear regression of `x` on age and returns the
#' residuals divided by their standard deviation.
#' @param x Numeric vector.
#' @param age Numeric vector of ages, of the same length as `x`.
#' @param sex Numeric vector of `0` or `1`, of the same length as `x`.
#' @param params Optional 2 × 3 matrix of the intercept, age coefficient, and
#' residual standard deviation (columns) for sex `0` and `1` (rows), such as the
#' `"params"` attribute returned by a previous call, used to standardize new data.
#' Default = `NULL`, in which case these are estimated from `x`.
#' @return Numeric vector of the same length as `x` containing the number of
#' standard deviations of each value from the mean for its age and sex. Missing
#' values are preserved in the output. If `params` is estimated, it is returned
#' in the `"params"` attribute.
#' @family other
#' @export
OR.standardize.agesex <- function(x, age, sex, params = NULL) {
  is.estimated <- is.null(params)
  if (is.estimated) {
    params <- matrix(NA, nrow = 2, ncol = 3,
                     dimnames = list(c("0", "1"), c("intercept", "age", "sd")))
    for (s in 0:1) {
      s.mask <- !is.na(sex) & sex == s
      fit <- stats::lm(x[s.mask] ~ age[s.mask])
      params[s + 1, ] <- c(stats::coef(fit), stats::sd(stats::residuals(fit)))
    }
  }
  output <- rep(NA, length(x))
  for (s in 0:1) {
    s.mask <- !is.na(sex) & sex == s
    output[s.mask] <- (x[s.mask] - params[s + 1, 1] - params[s + 1, 2] * age[s.mask]) /
      params[s + 1, 3]
  }
  if (is.estimated) attr(output, "params") <- params
  return(output)
}
