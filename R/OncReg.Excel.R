#' Convert mixed format dates to Microsoft Excel serial dates
#'
#' Converts string dates that may be in serial (using the Microsoft Excel
#' offset) or delimited format with a two-digit (e.g., `"31/01/00"`) or four-digit
#' (e.g., `"31/01/2000"`) year into numeric serial dates. Fields may be delimited
#' by `"/"`, `"-"` or `"."`. Two-digit years are expanded using a specified century
#' and pivot year.
#' @param input String vector of dates in serial or delimited format, where
#' fields are separated by `"/"`, `"-"` or `"."`.
#' @param century Numeric century used for expanding two-digit years. Default =
#' `20`.
#' @param pivot Numeric threshold where two-digit years > `pivot` are expanded
#' with `century - 1`. Default = `50`.
#' @param order String specifying the field order, e.g., `"dmy"`, `"ymd"`, or `"mdy"`.
#' Default = `"dmy"`.
#' @return Numeric vector of serial dates using the Microsoft Excel offset.
#' @examples
#' print(OR.date.Excel(c("31/01/00", "31-01-2000", "36556")))
#' @family other
#' @export
OR.date.Excel <- function (input, century = 20, pivot = 50, order = "dmy") {
  output <- input
  x <- OR.y.to.Y(input, century, pivot, order)
  o <- strsplit(order, "")[[1]]
  f <- paste0("%", ifelse(o == "y", "Y", o), collapse = "/")
  mask_n <- suppressWarnings(as.numeric(x))
  mask_d <- as.numeric(as.Date(x, f)) - as.numeric(as.Date("1899-12-30"))
  output[!is.na(mask_n)] <- mask_n[!is.na(mask_n)]
  output[!is.na(mask_d)] <- mask_d[!is.na(mask_d)]
  return(suppressWarnings(as.numeric(output)))
}

OR.days.per.month <- function(month, year) {
  n <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
  if (month == 2 && (year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0))) n <- 29
  return(n)
}

OR.y.to.Y <- function(input, century = 20, pivot = 50, order = "dmy", delimiters = c("-", ".")) {
  output <- input
  for (d in delimiters) {
    mask <- !is.na(output) & nchar(output) - nchar(gsub(d, "", output, fixed = TRUE)) == 2
    output[mask] <- gsub(d, "/", output[mask], fixed = TRUE)
  }
  o <- strsplit(order, "")[[1]]
  j <- which(o == "y")
  for (i in 1:length(output)) {
    s <- strsplit(output[i], "/", fixed = TRUE)[[1]]
    if (length(s) == 3) {
      if (nchar(gsub("[0-9]", "", s[1])) > 0 || nchar(gsub("[0-9]", "", s[2])) > 0 ||
          nchar(gsub("[0-9]", "", s[3])) > 0) {
        stop(paste0("[OR.y.to.Y] invalid date at element ", i, ": ", input[i]))
      }
      if (nchar(s[j]) == 2) {
        y <- s[j]
        if (as.numeric(y) <= pivot) {
          s[j] <- paste0(century, y)
        } else {
          s[j] <- paste0(century - 1, y)
        }
      }
      n <- as.numeric(s)
      year <- n[j]
      month <- n[which(o == "m")]
      day <- n[which(o == "d")]
      if (is.na(year) || is.na(month) || is.na(day) || year < 1 || month < 1 || month > 12 ||
          day < 1 || day > OR.days.per.month(month, year)) {
        stop(paste0("[OR.y.to.Y] invalid date at element ", i, ": ", input[i]))
      }
      output[i] <- paste0(n, collapse = "/")
    }
  }
  return(output)
}
