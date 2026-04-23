.OR.delim.contains <- function(x, match, delimiter, partial) {
  parts <- OR.delim.split(x, delimiter)
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

#' Detect delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' For each string in `x`, applies `OR.delim.split()` and checks whether any 
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
  if (is.data.frame(x)) x <- as.matrix(x)
  output <- sapply(x, function(x) .OR.delim.contains(x, match, delimiter, 
                                                     partial))
  if (is.matrix(x)) {
    dim(output) <- dim(x)
  } else {
    output <- unname(output)
  }
  return(output)
}

.OR.delim.intersect <- function(a, b, delimiter) {
  vector_a <- OR.delim.split(a, delimiter)
  vector_b <- OR.delim.split(b, delimiter)
  return(any(vector_a %in% vector_b))
}

#' Check if two delimited strings share any element (case-insensitive and 
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `a` and `b`, applies `OR.delim.split()`
#' and checks whether any element is shared between `a` and `b`.
#' @param a String vector, matrix, or data frame.
#' @param b String vector, matrix, or data frame of the same shape as `a`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A logical vector or matrix indicating whether `a` and `b` share any element.
#' @family delim
#' @export
OR.delim.intersect <- function(a, b, delimiter = ",") {
  if (is.data.frame(a)) a <- as.matrix(a)
  if (is.data.frame(b)) b <- as.matrix(b)
  if (is.null(dim(a)) && is.null(dim(b))) {
    if (length(a) != length(b)) stop("[OR.delim.intersect] a and b must have the same length")
  } else {
    if (!identical(dim(a), dim(b))) stop("[OR.delim.intersect] a and b must have the same shape")
  }
  output <- mapply(.OR.delim.intersect, a, b, delimiter, USE.NAMES = FALSE)
  if (is.matrix(a)) dim(output) <- dim(a) else output <- unname(output)
  return(output)
}

.OR.delim.merge <- function(a, b, delimiter) {
  p1 <- OR.delim.split(a, delimiter) 
  p2 <- OR.delim.split(b, delimiter) 
  parts <- c(p1, p2)
  parts <- parts[parts != ""]
  parts <- sort(unique(parts))
  return(paste(parts, collapse = paste0(delimiter, " ")))
}

#' Merge delimited string elements pairwise (case-insensitive and
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `a` and `b`, applies `OR.delim.split()`,
#' takes the union of the delimited elements, removes duplicate elements, sorts 
#' elements alphabetically, and rejoins the elements using the same delimiter.
#' @param a String vector, matrix, or data frame.
#' @param b String vector, matrix, or data frame of the same shape as `a`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector or matrix with merged delimited elements.
#' @family delim
#' @export
OR.delim.merge <- function (a, b, delimiter = ",") {
  if (is.data.frame(a)) a <- as.matrix(a)
  if (is.data.frame(b)) b <- as.matrix(b)
  if (is.null(dim(a)) && is.null(dim(b))) {
    if (length(a) != length(b)) 
      stop("[OR.delim.merge] a and b must have the same length")
  } else {
    if (!identical(dim(a), dim(b))) 
      stop("[OR.delim.merge] a and b must have the same shape")
  }
  output <- mapply(.OR.delim.merge, a, b, delimiter, USE.NAMES = FALSE)
  if (is.matrix(a)) {
    dim(output) <- dim(a)
  } else {
    output <- unname(output)
  }
  return(output)
}

.OR.delim.replace <- function(x, match, replacement, delimiter) {
  parts <- OR.delim.split(x, delimiter) 
  if (any(parts %in% tolower(trimws(match)))) {
    parts[parts %in% tolower(trimws(match))] <- tolower(trimws(replacement))
  }
  parts <- sort(unique(parts))
  parts <- parts[parts != ""]
  return(paste(parts, collapse = paste0(delimiter, " ")))
}

#' Replace delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' For each string in `x`, applies `OR.delim.split()` and checks whether any 
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
  if (is.data.frame(x)) x <- as.matrix(x)
  output <- sapply(x, function(t) .OR.delim.replace(t, match, replacement, delimiter))
  if (is.matrix(x)) {
    dim(output) <- dim(x)
  } else {
    output <- unname(output)
  }
  return(output)
}

#' Split delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' Splits a string by a specified delimiter, replaces `NA` with `""`, trims
#' whitespace, converts to lowercase, and returns delimited elements.
#' @param x String vector.
#' @param delimiter String delimiter. Default = `","`.
#' @return A character vector of delimited elements.
#' @family delim
#' @export
OR.delim.split <- function(x, delimiter = ",") {
  x <- OR.NA.to.empty(x)
  return(tolower(trimws(unlist(strsplit(x, delimiter, fixed = TRUE)))))
}

.OR.delim.subset <- function(a, b, delimiter = ",") {
  vector_a <- OR.delim.split(a, delimiter)
  vector_b <- OR.delim.split(b, delimiter)
  return(all(vector_b %in% vector_a))
}

#' Check if one delimited string is a subset of another (case-insensitive and 
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `a` and `b`, applies `OR.delim.split()` 
#' and checks whether all elements in `b` are 
#' contained in `a`.
#' @param a String vector, matrix, or data frame.
#' @param b String vector, matrix, or data frame of the same shape as `a`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A logical vector or matrix indicating whether `b` is a subset of `a`.
#' @family delim
#' @export
OR.delim.subset <- function(a, b, delimiter = ",") {
  if (is.data.frame(a)) a <- as.matrix(a)
  if (is.data.frame(b)) b <- as.matrix(b)
  if (is.null(dim(a)) && is.null(dim(b))) {
    if (length(a) != length(b)) stop("[OR.delim.subset] a and b must have the same length")
  } else {
    if (!identical(dim(a), dim(b))) stop("[OR.delim.subset] a and b must have the same shape")
  }
  output <- mapply(.OR.delim.subset, a, b, delimiter, USE.NAMES = FALSE)
  if (is.matrix(a)) dim(output) <- dim(a) else output <- unname(output)
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
