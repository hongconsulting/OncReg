#' Survival outcomes
#'
#' Computes event times and statuses from start, event, and review dates.
#' @param startdate Numeric vector of start dates.
#' @param eventdate Numeric vector of event dates.
#' @param reviewdate Numeric vector of latest follow-up dates.
#' @param divisor Unit conversion factor for time. Default = `365.2425/12`.
#' @param zero Logical. If `TRUE`, survival times equal to zero are allowed. 
#' Default = `FALSE`. 
#' @return A data frame with columns:
#' \describe{
#'   \item{time}{Survival time.}
#'   \item{status}{Event indicator (`1` = event, `0` = censored).}
#' }
#' Cases with missing start dates, or with invalid survival times (≤ `0`, or < `0`
#' when `zero = TRUE`), are returned as `NA` in both columns.
#' @family survival
#' @export
OR.survoutcome <- function(startdate, eventdate, reviewdate, 
                           divisor = 365.2425/12, zero = FALSE) {
  n <- length(startdate)
  output <- data.frame("time" = rep(NA, n), "status" = rep(NA, n))
  for (i in 1:n) {
    if (!is.na(startdate[i])) {
      if (!is.na(eventdate[i])) {
        output$time[i] <- (eventdate[i] - startdate[i])/divisor
        output$status[i] <- 1
      } else if (!is.na(reviewdate[i])) {
        output$time[i] <- (reviewdate[i] - startdate[i])/divisor
        output$status[i] <- 0
      }
    }
  }
  if (zero) {
    output$status[output$time < 0] <- NA
    output$time[output$time < 0] <- NA
  } else {
    output$status[output$time <= 0] <- NA
    output$time[output$time <= 0] <- NA
  }
  return(output)
}