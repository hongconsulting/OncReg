#' Last observation carried forward within clusters
#'
#' Applies LOCF imputation to `x` within each `id`, assuming observations are
#' already ordered within each `id`.
#' @param x Vector of values for LOCF imputation.
#' @param id Optional vector of cluster identifiers. If `NULL`, all observations
#' are treated as a single cluster.
#' @return Vector of same length as `x` with LOCF applied within each `id`.
#' @family imputation
#' @export
OR.LOCF.cluster <- function(x, id = NULL) {
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
