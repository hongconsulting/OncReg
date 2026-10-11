#' Summarise repeated measurements by ID
#'
#' Reduces a data frame to one row per ID, summarising each variable whose name
#' matches `pattern` by `method`. Non-matching variables are dropped.
#' @param data A data frame containing repeated measurements.
#' @param ID_varname Character string giving the name of the ID variable. Rows
#' with the same ID are treated as belonging to the same individual.
#' @param pattern String containing a regular expression used to select
#' variables. All variable names matching this pattern (via `grep()`) are
#' collapsed.
#' @param method Character string specifying how multiple values within each ID
#' should be collapsed. Options include:
#' \itemize{
#'   \item `"firstnm"`: Returns the first non-missing value in order of appearance.
#'   \item `"mean"`: Returns the mean of non-missing values.
#'   \item `"max"`: Returns the maximum of non-missing values.
#'   \item `"min"`: Returns the minimum of non-missing values.
#'   \item `"single"` (default): Returns the mean of non-missing values and prints
#'   a message if more than one unique non-missing value is found.
#'   \item `"sum"`: Returns the sum of non-missing values.
#' }
#' @details
#' Analogous to Stata's "collapse" command.
#'
#' An example use case is when working with REDCap exports, where the
#' measurements for one patient are distributed across multiple event rows.
#' @return A data frame with one row per unique ID and one column per variable
#' matching `pattern`, each summarised by `method`.
#' @family summary
#' @export
OR.collapse <- function(data, ID_varname, pattern, method = "single") {
  IDs  <- unique(data[[ID_varname]])
  varnames <- grep(pattern, names(data), value = TRUE)
  output <- data.frame("ID" = IDs)
  for (i in 1:length(varnames)) {
    varname <- varnames[i]
    output[[varname]] <- NA
    for (j in 1:length(IDs)) {
      ID <- IDs[j]
      ID_mask <- data[[ID_varname]] == ID
      subdata <- data[[varname]][ID_mask]
      subdata_na_omit <- OR.NA.rm(subdata)
      if (method != "single" & length(subdata_na_omit) == 0) {
        message("[OR.collapse] ", varname, " for ID ", ID, " is empty")
        next
      }
      subdata_unique <- unique(subdata_na_omit)
      if (method == "firstnm") {
        output[[varname]][j] <- subdata_na_omit[1]
      } else if (method == "max") {
        output[[varname]][j] <- max(subdata_unique)
      } else if (method == "mean" | method == "single") {
        if (method == "single" & length(subdata_unique) > 1) {
          message("[OR.collapse] ", varname, " for ID ", ID, " has more than 1 unique value\n")
        }
        output[[varname]][j] <- mean(subdata_na_omit)
      } else if (method == "min") {
        output[[varname]][j] <- min(subdata_unique)
      } else if (method == "sum") {
        output[[varname]][j] <- sum(subdata_na_omit)
      } else {
        stop(paste0("[OR.collapse] method ", method, " not implemented"))
      }
    }
  }
  return(output)
}

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

#' Statistical mode
#'
#' Returns the most common value in `x`.
#' @param x Vector.
#' @return A scalar containing the mode of `x`.
#' @family summary
#' @export
OR.mode <- function(x) {
  ux <- unique(x)
  output <- ux[which.max(tabulate(match(x, ux)))]
  return(output)
}

#' Frequency of the mode
#'
#' Returns the number of occurrences of the most common value in `x`.
#' @param x Vector.
#' @return Integer scalar giving the frequency of the mode.
#' @family summary
#' @export
OR.modefreq <- function(x) {
  ux <- unique(x)
  output <- max(tabulate(match(x, ux)))
  return(output)
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

#' Row-wise maxima below a threshold
#'
#' For each row of a matrix or data frame, returns the maximum value strictly
#' less than a corresponding threshold, ignoring `NA`s. Returns `NA` if no
#' values satisfy the condition.
#' @param input Numeric matrix or data frame.
#' @param threshold Numeric vector of thresholds of length equal to the number
#' of `input` rows.
#' @return Numeric vector of row-wise maxima below a threshold.
#' @family summary
#' @export
OR.rowmax.threshold <- function(input, threshold) {
  output <- apply(input, 1, function(x, t) {
    x <- x[!is.na(x) & x < t]
    if (length(x) == 0) NA else max(x)
  }, t = threshold)
  return(output)
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

#' Row-wise minima above a threshold
#'
#' For each row of a matrix or data frame, returns the minimum value strictly
#' greater than a corresponding threshold, ignoring `NA`s. Returns `NA` if no
#' values satisfy the condition.
#' @param input Numeric matrix or data frame.
#' @param threshold Numeric vector of thresholds of length equal to the number
#' of `input` rows.
#' @return Numeric vector of row-wise minima above a threshold.
#' @family summary
#' @export
OR.rowmin.threshold <- function(input, threshold) {
  output <- apply(input, 1, function(x, t) {
    x <- x[!is.na(x) & x > t]
    if (length(x) == 0) NA else min(x)
  }, t = threshold)
  return(output)
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
