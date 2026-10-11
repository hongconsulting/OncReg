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
