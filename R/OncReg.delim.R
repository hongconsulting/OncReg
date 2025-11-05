#' Detect delimited string elements (case- and whitespace-insensitive)
#'
#  Splits each string in a vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and checks whether any
#' delimited element matches one or more specified strings.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param match String vector of elements to match (case- and
#' whitespace-insensitive).
#' @param delimiter String delimiter. Default = `","`.
#' @return A logical vector or matrix indicating whether each string contains a
#' delimited element matching any of the specified strings.
#' @export
OR.delim.contains <- function(x, match, delimiter = ",") {
  f <- function(t) {
    parts <- tolower(trimws(unlist(strsplit(t, delimiter, fixed = TRUE))))
    return(any(parts %in% tolower(trimws(match))))
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

#' Replace delimited string elements (case- and whitespace-insensitive)
#'
#' Splits each string in a vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and checks whether any
#' delimited element matches one or more specified strings. Replaces any
#' matching elements with a new string, removes duplicate elements, sorts
#' elements alphabetically, and rejoins the elements using the same delimiter.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param match String vector of elements to match (case- and
#' whitespace-insensitive).
#' @param replacement String delimited element replacement (case- and
#' whitespace-insensitive).
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector or matrix with replaced delimited elements.
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
#' @export
OR.delim.replace <- function(x, match, replacement, delimiter = ",") {
  f <- function(t) {
    parts <- tolower(trimws(unlist(strsplit(t, delimiter, fixed = TRUE))))
    if (any(parts %in% tolower(trimws(match)))) {
      parts[parts %in% tolower(trimws(match))] <- tolower(trimws(replacement))
    }
    parts <- sort(unique(parts))
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

#' Frequency table of delimited string elements (case- and
#' whitespace-insensitive)
#'
#' Splits each string in a string vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and tabulates the
#' frequency of all unique delimited elements.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param delimiter String delimiter. Default = `","`.
#' @return A table of delimited element frequencies (case- and
#' whitespace-insensitive).
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' print(OR.delim.table(treatment))
#' @export
OR.delim.table <- function(x, delimiter = ",") {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) x <- as.vector(x)
  return(table(tolower(trimws(unlist(strsplit(x, delimiter, fixed = TRUE)))), useNA = "ifany"))
}
