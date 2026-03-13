OR.regimen.restart.single <- function (date_start, date_stop, reason_stop,
                                       date_prog, t_min, echo = FALSE) {
  t_duration <- date_stop - date_start
  t_interval <- rep(NA, length(date_start))
  for (i in 1:length(date_start)) {
    if (is.na(date_start[i])) next
    last_date_stop <- OR.max(date_stop[date_stop < date_start[i]])
    t_interval[i] <- date_start[i] - last_date_stop
  }
  # OR.F.to.NA() needed for which.min()
  restart_line <- as.numeric(which.min(date_start[OR.F.to.NA(t_interval >= t_min)]))
  if (echo) {
    cat("[OR.regimen.restart] date_start =", date_start, "\n")
    cat("[OR.regimen.restart] t_duration =", t_duration, "\n")
    cat("[OR.regimen.restart] date_stop =", date_stop, "\n")
    cat("[OR.regimen.restart] reason_stop =", reason_stop, "\n")
    cat("[OR.regimen.restart] t_interval =", t_interval, "\n")
    cat("[OR.regimen.restart] OR.F.to.NA(t_interval >= t_min) =", OR.F.to.NA(t_interval >= t_min), "\n")
  }
  if (length(restart_line) == 0) {
    prev_time <- OR.sum(t_duration)
    stop_reason <- OR.max(reason_stop)
    return(c(prev_time, stop_reason, rep(NA, 4)))
  }
  restart_date <- as.numeric(date_start[restart_line])
  prev_time <- OR.sum(t_duration[date_stop < restart_date])
  stop_reason <- OR.max(reason_stop[date_stop < restart_date])
  reprog_date <- OR.min(date_prog[date_prog > restart_date])
  if (echo) {
    cat("[OR.regimen.restart] date_stop < restart_date =", date_stop < restart_date, "\n")
    cat("[OR.regimen.restart] date_prog > restart_date =", date_prog > restart_date, "\n")
  }
  return(c(prev_time, stop_reason, t_interval[restart_line],
           restart_line, restart_date, reprog_date))
}

#' Identify treatment restart after a minimum treatment-free interval
#'
#' For each patient (rows), scans treatment lines (columns) to identify the
#' earliest line whose start date occurs at least `t_min` time units after the
#' most recent prior stop date. Also computes the cumulative treatment duration
#' before the restart, the maximum stop reason before the restart (for example,
#' `0` = stopped due to toxicity, `1` = stopped due to progressive disease), the
#' treatment-free interval, and the earliest progression date after the restart.
#' @param date_start Numeric matrix of treatment start dates.
#' @param date_stop Numeric matrix of treatment stop dates.
#' @param reason_stop Numeric matrix encoding the reason the treatment stopped.
#' @param date_prog Numeric matrix of progression dates.
#' @param t_min Minimum treatment-free interval required to qualify as a
#' restart. Default = `365.2425/2`.
#' @return Data frame with one row per patient and six columns:
#' \describe{
#' \item{prev_time}{Cumulative treatment duration before the restart.}
#' \item{stop_reason}{Maximum stopping reason code observed before the restart.}
#' \item{interval_time}{Interval between the latest stop and the restart.}
#' \item{restart_line}{Index of the treatment line where the restart occurs.}
#' \item{restart_date}{Start date of the restart line.}
#' \item{reprog_date}{Earliest progression date occurring after the restart.}
#' }
#' If no restart satisfying `t_min` is found, `interval_time`, `restart_line`,
#' `restart_date`, and `reprog_date` are returned as `NA`, while `prev_time`
#' and `stop_reason` summarize the full observed treatment history.
#' @export
OR.regimen.restart <- function(date_start, date_stop, reason_stop, date_prog,
                               t_min = 182.62125) {
  n <- nrow(date_start)
  output <- matrix(NA, n, 6)
  for (i in 1:n) {
    output[i, ] <- OR.regimen.restart.single(
      date_start[i, ],
      date_stop[i, ],
      reason_stop[i, ],
      date_prog[i, ],
      t_min
    )
  }
  output <- data.frame(output)
  colnames(output) <- c("prev_time", "stop_reason", "interval_time",
                        "restart_line", "restart_date", "reprog_date")
  return(output)
}

#' Truncate stop dates by maximum duration
#'
#' Limits the duration between `date_start` and `date_stop` to `t_max` by
#' truncating `date_stop` where necessary.
#' @param date_start Numeric matrix of start dates.
#' @param date_stop Numeric matrix of stop dates.
#' @param t_max Maximum allowed duration.
#' @return Numeric matrix of truncated stop dates where
#' `date_stop - date_start` does not exceed `t_max`.
#' @export
OR.regimen.truncate <- function(date_start, date_stop, t_max) {
  output <- date_stop
  for (row in 1:nrow(date_start)) {
    for (col in 1:ncol(date_start)) {
      if (is.na(date_start[row, col]) || is.na(date_stop[row, col])) next
      duration <- date_stop[row, col] - date_start[row, col]
      if (duration > t_max) output[row, col] <- date_start[row, col] + t_max
    }
  }
  return(output)
}
