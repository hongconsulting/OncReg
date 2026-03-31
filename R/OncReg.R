#' Remove zero elements
#'
#' Removes elements equal to `0` from a numeric vector.
#' @param x Numeric vector.
#' @return `x` with all elements equal to `0` removed.
#' @export
OR.0.rm <- function(x) {
  return(x[x != 0])
}

#' Convert strings to numeric after stripping non-numeric characters
#'
#' Removes all characters except digits, decimal point, and minus sign, then
#' converts the result to numeric. Non-convertible values return `NA` with
#' warnings suppressed.
#' @param x A string vector.
#' @return A numeric vector.
#' @export
OR.as.numeric <- function(x) {
  x <- gsub("[^0-9.\\-]", "", x)
  return(suppressWarnings(as.numeric(x)))
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

#' Convert mixed format dates to Microsoft Excel serial dates
#'
#' Converts string dates that may be in serial (using the Microsoft Excel
#' offset), "d/m/y" (e.g., "31/01/00"), or "d/m/Y" format (e.g., "31/01/2000")
#' into numeric serial dates. Two-digit years are expanded using a specified
#' century and pivot year.
#' @param input String vector of dates in serial, "d/m/y" or "d/m/Y" format.
#' @param century Numeric century (e.g., 20) used for expanding two-digit years.
#' @param pivot Numeric threshold where two-digit years > `pivot` are expanded
#' with `century - 1`.
#' @return Numeric vector of serial dates using the Microsoft Excel offset.
#' @examples
#' print(OR.dmyY.to.Excel(c("31/01/00", "31/01/2000", "36556"), 20, 25))
#' @export
OR.dmyY.to.Excel <- function(input, century, pivot) {
  output <- input
  x <- OR.y.to.Y(input, century, pivot)
  mask_n <- suppressWarnings(as.numeric(x))
  mask_d <- as.numeric(as.Date(x, "%d/%m/%Y")) - as.numeric(as.Date("1899-12-30"))
  output[!is.na(mask_n)] <- mask_n[!is.na(mask_n)]
  output[!is.na(mask_d)] <- mask_d[!is.na(mask_d)]
  return(suppressWarnings(as.numeric(output)))
}

#' Convert FALSE values to NA
#'
#' Converts all `FALSE` elements in a vector or matrix to `NA`.
#' @param x Logical vector or matrix.
#' @return The input object with all `FALSE` values replaced by `NA`.
#' @export
OR.F.to.NA <- function(x) {
  x[x == FALSE] <- NA
  return(x)
}

#' Maximum with NA removal or NA if no valid entries
#'
#' Equivalent to `max(x, na.rm = TRUE)` except returns `NA` if all elements are
#' `NA`.
#' @param x Numeric vector.
#' @return Maximum of non-`NA` elements or `NA` if none exist.
#' @export
OR.max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(max(x))
}

#' Convert mixed format dates to Microsoft Excel serial dates
#'
#' Converts string dates that may be in serial (using the Microsoft Excel
#' offset), "m/d/y" (e.g., "01/31/00"), or "m/d/Y" format (e.g., "01/31/2000")
#' into numeric serial dates. Two-digit years are expanded using a specified
#' century and pivot year.
#' @param input String vector of dates in serial, "m/d/y" or "m/d/Y" format.
#' @param century Numeric century (e.g., 20) used for expanding two-digit years.
#' @param pivot Numeric threshold where two-digit years > `pivot` are expanded
#' with `century - 1`.
#' @return Numeric vector of serial dates using the Microsoft Excel offset.
#' @examples
#' print(OR.mdyY.to.Excel(c("01/31/00", "01/31/2000", "36556"), 20, 25))
#' @export
OR.mdyY.to.Excel <- function (input, century, pivot) {
  output <- input
  x <- OR.y.to.Y(input, century, pivot)
  mask_n <- suppressWarnings(as.numeric(x))
  mask_d <- as.numeric(as.Date(x, "%m/%d/%Y")) - as.numeric(as.Date("1899-12-30"))
  output[!is.na(mask_n)] <- mask_n[!is.na(mask_n)]
  output[!is.na(mask_d)] <- mask_d[!is.na(mask_d)]
  return(suppressWarnings(as.numeric(output)))
}

#' Minimum with NA removal or NA if no valid entries
#'
#' Equivalent to `min(x, na.rm = TRUE)` except returns `NA` if all elements are
#' `NA`.
#' @param x Numeric vector.
#' @return Minimum of non-`NA` elements or `NA` if none exist.
#' @export
OR.min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(min(x))
}

#' Remove missing values
#'
#' Removes `NA` elements in a vector.
#' @param x Numeric vector.
#' @return `x` with all `NA` entries removed.
#' @export
OR.NA.rm <- function(x) {
  return(x[!is.na(x)])
}

#' Convert missing values to zero
#'
#' Converts all `NA` elements in a vector or matrix to `0`.
#' @param x Numeric vector or matrix.
#' @return The input object with all `NA` values replaced by `0`.
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
#' @export
OR.NA.to.F <- function (x) {
  x[is.na(x)] <- FALSE
  return(x)
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
#' @export
OR.rowmin <- function (input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else min(x)
  }))
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

#' Sum with NA removal or NA if no valid entries
#'
#' Equivalent to `sum(x, na.rm = TRUE)` except returns `NA` if all elements are
#' `NA`.
#' @param x Numeric vector.
#' @return Sum of non-`NA` elements or `NA` if none exist.
#' @export
OR.sum <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(sum(x))
}

#' Survival outcomes
#'
#' Computes event times and statuses from start, event, and review dates.
#' @param date_start Vector of start dates.
#' @param date_event Vector of event dates.
#' @param date_follow Vector of last follow-up dates for censored cases.
#' @param divisor Unit conversion factor for time. Default = `365.2425/12`.
#' @return A numeric matrix with two columns: survival time and status (`1` =
#' event, `0` = censored). Cases with missing start dates or with survival time
#' ≤ 0 are returned as `NA` in both columns.
#' @export
OR.survoutcome <- function(date_start, date_event, date_follow, divisor = 365.2425/12) {
  n <- length(date_start)
  output <- matrix(NA, n, 2)
  for (i in 1:n) {
    if (!is.na(date_start[i])) {
      if (!is.na(date_event[i])) {
        output[i, 1] <- (date_event[i] - date_start[i])/divisor
        output[i, 2] <- 1
        if (output[i, 1] <= 0) {
          output[i, 1] <- NA
          output[i, 2] <- NA
        }
      }
      else if (!is.na(date_follow[i])) {
        output[i, 1] <- (date_follow[i] - date_start[i])/divisor
        output[i, 2] <- 0
      }
    }
  }
  return(output)
}

OR.y.to.Y <- function(input, century, pivot) {
  output <- input
  s <- strsplit(input, "/", fixed = TRUE)
  for (i in 1:length(s)) {
    if (length(s[[i]]) == 3) {
      if (nchar(s[[i]][3]) == 2) {
        y <- s[[i]][3]
        if (as.numeric(y) <= pivot) {
          s[[i]][3] <- paste0(century, y)
        } else {
          s[[i]][3] <- paste0(century - 1, y)
        }
      }
      output[i] <- paste0(s[[i]], collapse = "/")
    }
  }
  return(output)
}
