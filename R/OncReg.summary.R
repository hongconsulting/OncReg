#' Maximum with NA removal or NA if no valid entries
#'
#' Equivalent to `max(x, na.rm = TRUE)` except returns `NA` (rather than `-Inf`) if
#' all elements are `NA`.
#' @param x Numeric vector.
#' @return Maximum of non-`NA` elements or `NA` if none exist.
#' @family summary
#' @export
OR.max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(max(x))
}

#' Arithmetic mean with NA removal or NA if no valid entries
#'
#' Equivalent to `mean(x, na.rm = TRUE)` except returns `NA` (rather than `NaN`)
#' if all elements are `NA`.
#' @param x Numeric vector.
#' @return Maximum of non-`NA` elements or `NA` if none exist.
#' @family summary
#' @export
OR.mean <- function (x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(mean(x))
}

#' Minimum with NA removal or NA if no valid entries
#'
#' Equivalent to `min(x, na.rm = TRUE)` except returns `NA` (rather than `Inf`)
#' if all elements are `NA`.
#' @param x Numeric vector.
#' @return Minimum of non-`NA` elements or `NA` if none exist.
#' @family summary
#' @export
OR.min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(min(x))
}

#' Row-wise leftmost non-missing value
#'
#' Returns the leftmost non-missing value in each row of a matrix or data frame.
#' Returns `NA` if an entire row is missing.
#' @details
#' An example use case is single imputation where column order defines the
#' hierarchy of preference, such as prioritizing tumor grade from the surgical
#' resection specimen and falling back to the core biopsy result if this is
#' missing.
#' @param input A matrix or data frame.
#' @return A vector of the leftmost non-missing values by row.
#' @family summary
#' @export
OR.rowleft <- function(input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else x[1]
  }))
}

#' Row-wise maxima
#'
#' Computes the maximum value of each row in a matrix or data frame, ignoring
#' `NA`s. Returns `NA` if an entire row is missing.
#' @details
#' An example use case in survival analysis is determining the date of last
#' follow-up from several dates when the patient was observed.
#' @param input A numeric matrix or data frame.
#' @return A numeric vector of row-wise maxima.
#' @family summary
#' @export
OR.rowmax <- function(input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else max(x)
  }))
}

#' Row-wise minima
#'
#' Computes the minimum value of each row in a matrix or data frame, ignoring
#' `NA`s. Returns `NA` if an entire row is missing.
#' @details
#' An example use case in survival analysis is determining the event date for
#' progression-free survival based on the dates of progression and death.
#' @param input A numeric matrix or data frame.
#' @return A numeric vector of row-wise minima.
#' @family summary
#' @export
OR.rowmin <- function (input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else min(x)
  }))
}

#' Sum with NA removal or NA if no valid entries
#'
#' Equivalent to `sum(x, na.rm = TRUE)` except returns `NA` (rather than `0`) if
#' all elements are `NA`.
#' @param x Numeric vector.
#' @return Sum of non-`NA` elements or `NA` if none exist.
#' @family summary
#' @export
OR.sum <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(sum(x))
}
