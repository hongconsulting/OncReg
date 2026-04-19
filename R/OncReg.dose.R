#' Minimum-cost vial combination to achieve a target dose
#'
#' Solves a minimum-cost unbounded covering knapsack problem by brute-force
#' enumeration to find vial combinations of `denominations` that achieve at least
#' `target` dose with minimum total cost, breaking ties by the smallest number of vials.
#' @param denominations Numeric vector of vial sizes.
#' @param target Target total dose.
#' @param costs Optional numeric vector of costs per vial. If `NULL`, assumed
#' proportional to `denominations`.
#' @return A data frame of optimal combinations with columns:
#' \itemize{
#'   \item `n1, n2, ..., nk`: counts per vial denomination
#'   \item `sum_n`: total number of vials
#'   \item `sum_amount`: total dose
#'   \item `diff_amount`: excess over target
#'   \item `sum_cost`: total cost
#' }
#' @family dose
#' @export
OR.dose.vials <- function(denominations, target, costs = NULL) {
  # if no prices specified, assume 1 unit cost per 1 unit amount:
  if (is.null(costs)) costs <- denominations
  # remove missing entries:
  nonmissing <- !is.na(denominations)
  denominations <- denominations[nonmissing]
  costs <- costs[nonmissing]
  # calculate search limits:
  k <- length(denominations)
  n <- ceiling(target / denominations)
  # generate every plausible combination of denominations:
  counts <- as.matrix(do.call(expand.grid, lapply(seq_len(k), function(i) 0:n[i])))
  colnames(counts) <- paste0("n", seq_len(k))
  # calculate summary statistics for each possible combination:
  sum_amount <- as.numeric(counts %*% denominations)
  sum_cost <- as.numeric(counts %*% costs)
  diff_amount <- sum_amount - target
  output <- data.frame(cbind(counts, sum_n = rowSums(counts),
                             sum_amount = sum_amount, diff_amount = diff_amount,
                             sum_cost = sum_cost))
  # select only combinations that are at least the target amount:
  output <- output[output$diff_amount >= 0,]
  # select only combinations with the lowest cost:
  output <- output[output$sum_cost == min(output$sum_cost),]
  # select only combinations with the lowest number of vials:
  output <- output[output$sum_n == min(output$sum_n),]
  return(output)
}

#' Stabilize and impute dosing weights within clusters
#'
#' Within each `id`, carries forward the most recent non-missing value of `x`,
#' treating small relative changes (within `threshold`) as unchanged, assuming
#' observations are already ordered within each `id`.
#'
#' Zeros in `x` are treated as missing. If the first value within a cluster is
#' missing, it is initialized to the first non-missing value in that cluster.
#'
#' For example, with `threshold = 0.1`, changes within ±10% are ignored:
#' `c(0, 100, NA, 105, 110)` is replaced with `c(100, 100, 100, 100, 110)`.
#' @param x Numeric vector of sorted weights.
#' @param id Optional vector of cluster identifiers. If `NULL`, all observations
#' are treated as a single cluster.
#' @param threshold Relative change threshold for updating the current value.
#' Default = `0`.
#' @return Numeric vector of same length as `x` with stabilized and imputed
#' weights.
#' @family dose
#' @export
OR.dose.weights <- function(x, id = NULL, threshold = 0) {
  x[x == 0] <- NA # weights == 0 are invalid
  if (is.null(id)) id <- rep(0, length(x))
  output <- rep(NA, length(x))
  unique_id <- unique(id)
  for (i in seq_along(unique_id)) {
    subset_indices <- which(id == unique_id[i])
    subset_x <- x[subset_indices]
    current_x <- OR.rowleft(t(as.matrix(subset_x))) # earliest non-missing x
    for (j in seq_along(subset_x)) {
      if (is.na(subset_x[j])) {
        subset_x[j] <- current_x # fill in missing values
      } else if ((subset_x[j]/current_x < 1 + threshold) & (subset_x[j]/current_x > 1 - threshold)) {
        subset_x[j] <- current_x # ignore weight changes below threshold
      } else {
        current_x <- subset_x[j]
      }
    }
    output[subset_indices] <- subset_x
  }
  return(output)
}
