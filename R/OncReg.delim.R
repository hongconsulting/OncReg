#' Detect delimited string elements (case-insensitive and whitespace-trimmed)
#'
#  Splits each string in a vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and checks whether any
#' delimited element matches one or more specified strings.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param match String vector of elements to match.
#' @param delimiter String delimiter. Default = `","`.
#' @param partial Logical. If `TRUE`, each delimited element is tested for
#' substring matches against the provided `match` strings. If `FALSE` (default),
#' matches must be exact.
#' @return A logical vector or matrix indicating whether each string contains a
#' delimited element matching any of the specified strings.
#' @family delim
#' @export
OR.delim.contains <- function(x, match, delimiter = ",", partial = FALSE) {
  f <- function(t) {
    parts <- tolower(trimws(unlist(strsplit(t, delimiter, fixed = TRUE))))
    match_ <- tolower(trimws(match))
    if (partial) {
      for (p in parts) {
        for (m in match_) {
          if (grepl(m, p, fixed = TRUE)) return(TRUE)
        }
      }
      return(FALSE)
    } else {
      return(any(parts %in% match_))
    }
  }
  if (is.data.frame(x)) x <- as.matrix(x)
  output <- sapply(x, f)
  if (is.matrix(x)) {
    dim(output) <- dim(x)
  } else {
    output <- unname(output)
  }
  return(output)
}

#' Merge delimited string elements pairwise (case-insensitive and
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `x1` and `x2`, splits the strings by
#' a specified delimiter, trims whitespace, converts to lowercase, takes the
#' union of the delimited elements, removes duplicate elements, sorts elements
#' alphabetically, and rejoins the elements using the same delimiter.
#' @param x1 String vector, matrix, or data frame.
#' @param x2 String vector, matrix, or data frame of the same shape as `x1`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector or matrix with merged delimited elements.
#' @family delim
#' @export
OR.delim.merge <- function(x1, x2, delimiter = ",") {
  if (is.data.frame(x1)) x1 <- as.matrix(x1)
  if (is.data.frame(x2)) x2 <- as.matrix(x2)
  if (is.null(dim(x1)) && is.null(dim(x2))) {
    if (length(x1) != length(x2)) stop("[OR.delim.merge] x1 and x2 must have the same length")
  } else {
    if (!identical(dim(x1), dim(x2))) stop("[OR.delim.merge] x1 and x2 must have the same shape")
  }
  f <- function(a, b) {
    p1 <- tolower(trimws(unlist(strsplit(a, delimiter, fixed = TRUE))))
    p2 <- tolower(trimws(unlist(strsplit(b, delimiter, fixed = TRUE))))
    parts <- c(p1, p2)
    parts <- parts[parts != ""]
    parts <- sort(unique(parts))
    return(paste(parts, collapse = paste0(delimiter, " ")))
  }
  output <- mapply(f, x1, x2, USE.NAMES = FALSE)
  if (is.matrix(x1)) {
    dim(output) <- dim(x1)
  } else {
    output <- unname(output)
  }
  return(output)
  if (length(x2) == 1L) {
    output <- sapply(x1, function(a) f(a, x2))
  } else {
    output <- mapply(f, x1, x2, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
  if (is.matrix(x1)) {
    dim(output) <- dim(x1)
  } else {
    output <- unname(output)
  }
  return(output)
}

#' Replace delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' Splits each string in a vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and checks whether any
#' delimited element matches one or more specified strings. Replaces any
#' matching elements with a new string, removes duplicate elements, sorts
#' elements alphabetically, and rejoins the elements using the same delimiter.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param match String vector of elements to match.
#' @param replacement String delimited element replacement.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector or matrix with replaced delimited elements.
#' @family delim
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' treatment <- OR.delim.replace(treatment, "anastrozole", "ai")
#' treatment <- OR.delim.replace(treatment, "letrozole", "ai")
#' treatment <- OR.delim.replace(treatment, "palbociclib", "cdk46i")
#' treatment <- OR.delim.replace(treatment, "ribociclib", "cdk46i")
#' print(treatment)
#' @family delim
#' @export
OR.delim.replace <- function(x, match, replacement, delimiter = ",") {
  f <- function(t) {
    parts <- tolower(trimws(unlist(strsplit(t, delimiter, fixed = TRUE))))
    if (any(parts %in% tolower(trimws(match)))) {
      parts[parts %in% tolower(trimws(match))] <- tolower(trimws(replacement))
    }
    parts <- sort(unique(parts))
    parts <- parts[parts != ""]
    return(paste(parts, collapse = paste0(delimiter, " ")))
  }
  if (is.data.frame(x)) x <- as.matrix(x)
  output <- sapply(x, f)
  if (is.matrix(x)) {
    dim(output) <- dim(x)
  } else {
    output <- unname(output)
  }
  return(output)
}

#' Frequency table of delimited string elements (case-insensitive and
#' whitespace-trimmed)
#'
#' Splits each string in a string vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and tabulates the
#' frequency of all unique delimited elements.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param delimiter String delimiter. Default = `","`.
#' @return A table of delimited element frequencies.
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' print(OR.delim.table(treatment))
#' @family delim
#' @export
OR.delim.table <- function(x, delimiter = ",") {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) x <- as.vector(x)
  return(table(tolower(trimws(unlist(strsplit(x, delimiter, fixed = TRUE)))), useNA = "ifany"))
}
