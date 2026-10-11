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

#' Identify element-wise mismatches between two vectors, treating NAs as equal to each other
#'
#' Returns the elements that differ between two vectors. Unlike `==`, which
#' returns `NA` whenever either operand is `NA`, missing values are treated as
#' equal to each other and unequal to everything else.
#' @param a A vector.
#' @param b A vector of the same length as `a`.
#' @return A two-column matrix of the mismatching elements, with `a` in the
#' first column and `b` in the second.
#' @family basic
#' @export
OR.check.equal <- function(a, b) {
  equal <- (is.na(a) & is.na(b)) |
    (!is.na(a) & !is.na(b) & a == b)
  return(cbind(a, b)[which(!equal), ])
}

#' Remove constant columns
#'
#' Removes any column from a `data.frame` where every observation is identical.
#' `NA` is treated as a value, so all-`NA` columns are removed, but a column of
#' one value mixed with `NA` is kept.
#' @param df A `data.frame`.
#' @return A `data.frame` with constant columns removed.
#' @family basic
#' @export
OR.colconst.rm <- function(df) {
  return(df[, vapply(df, function(x) length(unique(x)) > 1L, logical(1)), drop = FALSE])
}

#' Remove all-NA columns
#'
#' Removes any column from a `data.frame` where every value is `NA`.
#' @param df A `data.frame`.
#' @return A `data.frame` with all-`NA` columns removed.
#' @family basic
#' @export
OR.colNA.rm <- function(df) {
  return(df[, vapply(df, function(x) !all(is.na(x)), logical(1)), drop = FALSE])
}

#' Rebase dates within clusters
#'
#' Within each `id`, re-expresses `date` as elapsed time from that id's
#' earliest value, so that measurements count from `0` onwards.
#' @param id vector of cluster identifiers.
#' @param date Numeric or `Date` vector of dates.
#' @return Numeric vector of elapsed time from each `id`'s baseline (minimum)
#' value.
#' @family datetime
#' @export
OR.date.rebase <- function(id, date) {
  return(as.numeric(stats::ave(as.numeric(date), id, FUN = function(x) x - OR.min(x))))
}

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

#' Merge with one-to-many widening
#'
#' Merges `x` with `y` where each row of `x` may match several rows of `y`.
#' Matching rows of `y` are numbered in their existing order and reshaped wide,
#' so each column of `y` appears once per match, suffixed `1`, `2`, ....
#' @param x A `data.frame`.
#' @param y A `data.frame` with possibly several rows per key.
#' @param by.x Name of the key column in `x`.
#' @param by.y Name of the key column in `y`.
#' @param all Logical; passed to [merge()] (default `TRUE`, full outer join).
#' @param sep Separator between column name and suffix (default `""`).
#' @return A `data.frame` with one row per key.
#' @family basic
#' @export
OR.merge.wide <- function(x, y, by.x, by.y, all = TRUE, sep = "") {
  # check key columns exist and y has no missing keys
  if (base::is.na(base::match(by.x, base::names(x)))) base::stop("`by.x` is not a column of `x`.")
  if (base::is.na(base::match(by.y, base::names(y)))) base::stop("`by.y` is not a column of `y`.")
  if (base::anyNA(y[[by.y]])) base::stop("`y` has missing values in `by.y`.")
  # number rows of y within each key: 1, 2, ...
  y$.seq <- stats::ave(base::seq_len(base::nrow(y)), y[[by.y]], FUN = base::seq_along)
  # widen y to one row per key, then merge
  return(base::merge(
    x = x,
    y = stats::reshape(y, idvar = by.y, timevar = ".seq", direction = "wide", sep = sep),
    by.x = by.x, by.y = by.y, all = all
  ))
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

#' Row-bind data frames with different columns
#'
#' Combines two data frames by row, matching columns by name. Columns missing
#' from one data frame are filled with `NA`.
#' @param df.1 A data frame.
#' @param df.2 A data frame.
#' @return A data frame with the rows of `df.1` followed by the rows of `df.2`.
#' @family basic
#' @export
OR.rbind.fill <- function(df.1, df.2) {
  if (!is.data.frame(df.1) || !is.data.frame(df.2)) {
    stop("[OR.rbind.fill] both inputs must be data.frames")
  }
  # add the columns each input lacks, filled with NA of the other's type
  for (current.name in setdiff(names(df.2), names(df.1))) {
    df.1[[current.name]] <- df.2[[current.name]][rep(NA_integer_, nrow(df.1))]
  }
  for (current.name in setdiff(names(df.1), names(df.2))) {
    df.2[[current.name]] <- df.1[[current.name]][rep(NA_integer_, nrow(df.2))]
  }
  # rbind matches columns by name and handles the types
  return(rbind(df.1, df.2))
}

#' Coalesce unanimous rows
#'
#' Within each group defined by `id`, coalesces the rows into a single row if
#' every column is unanimous, i.e. its non-`NA` values all agree. The
#' coalesced row takes the agreed value in each column. Groups with any
#' disagreement are returned unchanged.
#' @param df A `data.frame`.
#' @param id Vector of group identifiers, one per row of `df`. For composite
#'   keys, combine them first, e.g. `paste(df$a, df$b)`.
#' @param ignore Character vector of column names left out of the unanimity
#'   check; the coalesced row takes their value from the group's first row
#'   (default `NULL`).
#' @return A `data.frame` with unanimous groups coalesced to one row.
#' @family basic
#' @export
OR.rowcoalesce <- function(df, id, ignore = NULL) {
  # check inputs
  if (length(id) != nrow(df)) stop("`id` must have one element per row of `df`.")
  if (anyNA(id)) stop("`id` contains missing values.")
  if (anyNA(match(ignore, names(df)))) stop("`ignore` contains names not in `df`.")
  # integer group index
  group <- match(id, unique(id))
  # first row of each group, indexed by group
  first.row <- which(!duplicated(group))
  # columns checked for unanimity (key columns, if present, are trivially unanimous)
  check.cols <- names(df)[is.na(match(names(df), ignore))]
  # flag groups where any checked column has more than one distinct non-NA value
  is.conflict <- rep(FALSE, length(first.row))
  for (col in check.cols) {
    is.present <- !is.na(df[[col]])
    is.distinct <- !duplicated(data.frame(group = group, value = df[[col]])[is.present, ])
    is.conflict <- is.conflict | tabulate(group[is.present][is.distinct], nbins = length(first.row)) > 1L
  }
  # in unanimous groups, copy each checked column's first non-NA value into the group's first row
  for (col in check.cols) {
    source.row <- which(!is.na(df[[col]]))
    source.row <- source.row[!duplicated(group[source.row]) & !is.conflict[group[source.row]]]
    df[[col]][first.row[group[source.row]]] <- df[[col]][source.row]
  }
  # keep every row of conflicting groups, and only the first row of unanimous groups
  return(df[is.conflict[group] | !duplicated(group), , drop = FALSE])
}

#' Remove subsumed rows
#'
#' Removes duplicate rows, then any row whose non-`NA` values all match another
#' row, treating `NA` as matching anything. Ignored columns are treated as `NA`
#' throughout but are kept in the output, taking the value from whichever
#' retained row they came from.
#' @param df A `data.frame`.
#' @param ignore Character vector of column names left out of the comparison
#'   (default `NULL`).
#' @return A `data.frame` with duplicate and subsumed rows removed.
#' @family basic
#' @export
OR.rowsubsumed.rm <- function(df, ignore = NULL) {
  # check ignored columns exist and at least one column remains informative
  if (anyNA(match(ignore, names(df)))) stop("`ignore` contains names not in `df`.")
  is.informative <- is.na(match(names(df), ignore))
  if (sum(is.informative) == 0L) stop("`ignore` cannot include every column.")
  # drop rows duplicated on informative columns (NA equals NA); required by the duplicated() step below
  df <- df[!duplicated(df[is.informative]), , drop = FALSE]
  # observed-value indicator matrix with ignored columns forced unobserved,
  # and one NA-pattern string per row
  is.observed <- !is.na(df)
  is.observed[, !is.informative] <- FALSE
  row.pattern <- do.call(paste0, as.data.frame(1L * is.observed))
  keep.row <- rep(TRUE, nrow(df))
  for (pattern in unique(row.pattern)) {
    in.pattern <- row.pattern == pattern
    pattern.cols <- is.observed[which(in.pattern)[1L], ]
    # candidates: observed on every column in pattern.cols, plus at least one more
    is.candidate <- rowSums(is.observed[, pattern.cols, drop = FALSE]) == sum(pattern.cols) &
      rowSums(is.observed) > sum(pattern.cols)
    if (sum(pattern.cols) == 0L) {
      # all-NA rows are subsumed by any row with an observed value
      keep.row[in.pattern] <- sum(is.candidate) == 0L
    } else if (sum(is.candidate) > 0L) {
      # subsumed if values on pattern.cols exactly match some candidate's (candidates stacked first);
      # same-pattern rows cannot match each other because duplicates were removed above
      keep.row[in.pattern] <- !duplicated(rbind(
        df[is.candidate, pattern.cols, drop = FALSE],
        df[in.pattern, pattern.cols, drop = FALSE]
      ))[-seq_len(sum(is.candidate))]
    }
  }
  return(df[keep.row, , drop = FALSE])
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
