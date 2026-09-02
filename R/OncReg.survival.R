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

#' Cox regression with time-dependent covariates
#'
#' Fits a Cox proportional-hazards model with one or more binary time-dependent
#' covariates, each taking the value `0` before its transition time and `1`
#' thereafter. Follow-up is split at each transition time, so that time at risk
#' preceding the transition is correctly assigned to the unexposed state,
#' avoiding immortal time bias.
#' @param id Vector of subject identifiers.
#' @param time Numeric vector of follow-up times.
#' @param status Numeric vector of event indicators, where `1` denotes the event
#' and `0` denotes censoring.
#' @param X.tdc Data frame of transition times for each time-dependent
#' covariate, with `NA` where the transition does not occur. Column names are
#' used as covariate names.
#' @param X Optional data frame of time-independent covariates. Default = `NULL`.
#' @return An object of class `coxph`.
#' @examples
#' set.seed(24601)
#' n <- 500
#' data <- data.frame("id" = 1:n)
#' data$date.event <- rexp(n, rate = log(2)/6)
#' data$date.censor <- rexp(n, rate = log(2)/12)
#' data$time <- pmin(data$date.event, data$date.censor)
#' data$status <- as.numeric(data$date.event <= data$date.censor)
#' data$date.cycle1 <- 1
#' data$date.cycle1[runif(n) < 0.05] <- NA
#' data$date.cycle1[data$date.cycle1 > data$time] <- NA
#' OR.tdCox(data$id, data$time, data$status, X.tdc = data.frame("cycle1" = data$date.cycle1))
#' @family survival
#' @export
OR.tdCox <- function(id, time, status, X.tdc, X = NULL) {
  data <- data.frame("id" = id, "survtime" = time, "survstatus" = status)
  if (is.null(X)) {
    dataC <- NULL
  } else {
    dataC <- data.frame(X)
    data <- cbind(data, dataC)
  }
  dataTDC <- data.frame(X.tdc)
  data <- survival::tmerge(data, data, id = id, event = event(survtime, survstatus))
  for (i in 1:ncol(dataTDC)) {
    temp <- data.frame("id" = id, "time" = dataTDC[, i])
    data <- survival::tmerge(data, temp, id = id, tdc = tdc(time))
    names(data)[names(data) == "tdc"] <- names(dataTDC)[i]
  }
  formula <- paste0("Surv(tstart, tstop, event) ~ ",
                    paste0(c(colnames(dataC), colnames(dataTDC)), collapse = " + "))
  # cat(formula, "\n")
  fit <- survival::coxph(stats::as.formula(formula), data = data, mode = TRUE)
  return(fit)
}
