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
#' @family survival
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
