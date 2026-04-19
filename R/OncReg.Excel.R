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
#' @family Excelserialdates
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
#' @family Excelserialdates
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
