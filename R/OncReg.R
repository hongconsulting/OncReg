#' Remove zero elements
#'
#' Removes elements equal to `0` from a numeric vector.
#' @param x Numeric vector.
#' @return `x` with all elements equal to `0` removed.
#' @family basic
#' @export
OR.0.rm <- function(x) {
  return(x[x != 0])
}

#' Convert strings to numeric after normalising minus signs and stripping digit 
#' grouping separators
#'
#' Replaces Unicode minus signs (`"\u2212"`) with ASCII `"-"`, removes either 
#' commas or spaces used as digit grouping (thousands) separators, then converts 
#' the result to numeric. Non-convertible values return `NA` with warnings 
#' suppressed.
#' @param x A string vector.
#' @return A numeric vector.
#' @family basic
#' @export
OR.as.numeric <- function(x) {
  x <- gsub("\u2212", "-", x, fixed = TRUE)
  x1 <- gsub("(?<=\\d),(?=\\d{3}(\\D|$))", "", x, perl = TRUE)
  x2 <- gsub("(?<=\\d) (?=\\d{3}(\\D|$))", "", x, perl = TRUE)
  x1 <- suppressWarnings(as.numeric(x1))
  x2 <- suppressWarnings(as.numeric(x2))
  return(OR.rowleft(cbind(x1, x2)))
}


# #' Standardize a numeric vector
# #'
# #' #' Returns *z*-scores by subtracting the sample mean and dividing by the
# #' sample standard deviation. Missing values are preserved in the output.
# #' @param x Numeric vector.
# #' @return Numeric vector of the same length as `x` containing the standardized
# #' values.
# #' @export
# OR.standardize <- function(x) {
#   mu <- mean(x, na.rm = TRUE)
#   sd <- stats::sd(x, na.rm = TRUE)
#   return((x - mu) / sd)
# }

#' Convert FALSE values to NA
#'
#' Converts all `FALSE` elements in a vector or matrix to `NA`.
#' @param x Logical vector or matrix.
#' @return The input object with all `FALSE` values replaced by `NA`.
#' @family basic
#' @export
OR.F.to.NA <- function(x) {
  x[x == FALSE] <- NA
  return(x)
}

#' Remove missing values
#'
#' Removes `NA` elements in a vector.
#' @param x Numeric vector.
#' @return `x` with all `NA` entries removed.
#' @family basic
#' @export
OR.NA.rm <- function(x) {
  return(x[!is.na(x)])
}

#' Convert missing values to zero
#'
#' Converts all `NA` elements in a vector or matrix to `0`.
#' @param x Numeric vector or matrix.
#' @return The input object with all `NA` values replaced by `0`.
#' @family basic
#' @export
OR.NA.to.0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

#' Convert missing values to empty strings
#'
#' Converts all `NA` elements in a vector or matrix to `""`.
#' @param x String vector or matrix.
#' @return The input object with all `NA` values replaced by `""`.
#' @family basic
#' @export
OR.NA.to.empty <- function(x) {
  x[is.na(x)] <- ""
  return(x)
}

#' Convert missing values to FALSE
#'
#' Converts all `NA` elements in a vector or matrix to `FALSE`.
#' @param x Logical vector or matrix.
#' @return The input object with all `NA` values replaced by `FALSE`.
#' @family basic
#' @export
OR.NA.to.F <- function (x) {
  x[is.na(x)] <- FALSE
  return(x)
}

#' Convert string variable to binary variable
#'
#' Converts a string vector to a numeric vector according to the following:
#' 1. Elements exactly matching any value in `missing` return `NA`.
#' 2. Otherwise, elements matching any fixed pattern in `pattern` return `1`.
#' 3. All other elements return `0`.
#' @param input String vector to evaluate.
#' @param pattern String vector of fixed patterns to each within `input`.
#' @param missing String vector of exact values to treat as `NA`.
#' @return Numeric vector of `1` (pattern matched), `0` (no match), or `NA`
#' (missing).
#' @examples
#' print(OR.string.to.binary(c("Yes", "Yes*", "No", "Missing"), "Yes", "Missing"))
#' @family basic
#' @export
OR.string.to.binary <- function(input, pattern, missing) {
  output <- rep(NA, length(input))
  for (i in 1:length(input)) {
    if (input[i] %in% missing) {
      output[i] <- NA
    } else if (any(grepl(pattern, input[i], fixed = TRUE))) {
      output[i] <- 1
    } else {
      output[i] <- 0
    }
  }
  return(output)
}
