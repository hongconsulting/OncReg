#' Identify line of restarted treatment
#'
#' For each patient, identifies the first line of treatment whose start date
#' occurs at least a specified number of days after any earlier stop date.
#' @param start.dates Numeric matrix of treatment start dates, with one row per
#' patient and one column per line of treatment.
#' @param stop.dates Numeric matrix of treatment stop dates, with one row per
#' patient and one column per line of treatment.
#' @param min.days Minimum number of days between a stop date and a start date
#' required to define a treatment restart. Default = `182.62125`.
#' @return A numeric matrix with two columns: the first column indicating the
#' line of restarted treatment and the second column indicating the number of
#' days since the previous stop date.
#' @export
OR.line.restart <- function(start.dates, stop.dates, min.days = 182.62125) {
  if (ncol(start.dates) != ncol(stop.dates)) stop("[OR.line.findrestart] inconsistent number of lines of treatments")
  if (nrow(start.dates) != nrow(stop.dates)) stop("[OR.line.findrestart] inconsistent number of patients")
  n.patients <- nrow(start.dates)
  n.lines <- ncol(start.dates)
  output <- matrix(NA, n.patients, 2)
  for (i in 1:n.patients) {
    for (j in 1:n.lines) {
      if (is.na(start.dates[i, j])) next
      prev.stop.dates <- stop.dates[i,]
      prev.stop.dates <- prev.stop.dates[!is.na(prev.stop.dates)]
      prev.stop.dates <- prev.stop.dates[prev.stop.dates < start.dates[i, j]]
      if (length(prev.stop.dates) == 0) next
      delta <- as.numeric(start.dates[i, j] - max(prev.stop.dates))
      if (delta >= min.days) {
        output[i, 1] <- j
        output[i, 2] <- delta
        break
      }
    }
  }
  return(output)
}

#' Truncate stop dates for lines of treatment
#'
#' For each line of treatment, truncates the recorded stop date so that it does
#' not exceed a specified number of days after its start date.
#' @param start.dates Numeric matrix of treatment start dates, with one row per
#' patient and one column per line of treatment.
#' @param stop.dates Numeric matrix of treatment stop dates, with one row per
#' patient and one column per line of treatment.
#' @param max.days Maximum number of days allowed between each start and stop
#' date. Default = `168`.
#' @details
#' An example use case is with oxaliplatin-containing chemotherapy regimens such
#' as FOLFOX where the oxaliplatin component is usually limited to no more than
#' 24 weeks and time beyond this represents treatment with 5-fluorouracil only.
#' @return A matrix of truncated stop dates.
#' @export
OR.line.truncate <- function(start.dates, stop.dates, max.days = 168) {
  output <- stop.dates
  for (i in 1:nrow(stop.dates)) {
    for (j in 1:ncol(stop.dates)) {
      if (!is.na(start.dates[i, j]) && !is.na(stop.dates[i, j])) {
        output[i, j] <- min(stop.dates[i, j], start.dates[i, j] + max.days)
      }
    }
  }
  return(output)
}
