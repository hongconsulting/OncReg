#' Difference in Kaplan–Meier survival at a fixed time point
#'
#' Compares Kaplan–Meier survival at a fixed time point between each arm and
#' the baseline arm (the lowest value of `group`). Confidence intervals use
#' unpooled Greenwood¹ variances on the identity scale and *p*-values use Wald
#' tests on the complementary log–log scale².
#' @param time Numeric vector of follow-up times.
#' @param status Numeric vector of event indicators, where `1` denotes the event
#' and `0` denotes censoring.
#' @param group Numeric vector of arm indicators. The lowest value is the
#' baseline arm.
#' @param fixedt Numeric time point at which survival is compared.
#' @param conf.level Numeric confidence level. Default = `0.95`.
#' @param LOCF Logical. If `TRUE`, survival beyond an arm's last observation is
#' carried forward³. If `FALSE`, it is `NA` unless the Kaplan–Meier estimate has
#' reached `0`. Default = `FALSE`.
#' @return A numeric matrix with one row per non-baseline arm, named by its
#' `group` value, and columns:
#' \describe{
#'   \item{diff}{Survival difference, arm minus baseline.}
#'   \item{SE}{Standard error of the difference.}
#'   \item{LL}{Lower confidence limit.}
#'   \item{UL}{Upper confidence limit.}
#'   \item{p}{Two-sided *p*-value.}
#' }
#' @details
#' Where survival reaches `0`, the standard error is set to `0` (whereas
#' `survival::survfit()` returns `NaN`). This keeps results usable in downstream
#' pooling (e.g., multiple imputation), where a single `NaN` would propagate to
#' the pooled estimate. When either arm's survival is `0` or `1`, the
#' complementary log–log transform is undefined and the *p*-value falls back to
#' the identity-scale Wald test. When both standard errors are `0`, the *p*-value
#' is `1` if the arms are equal and `0` otherwise.
#' @references
#' 1. Greenwood, M., 1926. A report on the natural duration of cancer. In:
#' *Reports on Public Health and Medical Subjects*, 33, pp. 1–26. London:
#' Her Majesty’s Stationery Office, Ministry of Health.
#' 2. Klein, J.P., Logan, B., Harhoff, M. and Andersen, P.K., 2007. Analyzing
#' survival curves at a fixed point in time. *Statistics in Medicine*, 26(24),
#' pp. 4505–4519.
#' 3. Gill, R.D., 1983. Large sample behaviour of the product-limit estimator on
#' the whole line. *The Annals of Statistics*, 11(1), pp. 49–58.
#' @family survival
#' @export
OR.KMdiff <- function(time, status, group, fixedt, conf.level = 0.95,
                      LOCF = FALSE) {
  arms    <- sort(unique(group))
  n.arms  <- length(arms)
  surv    <- numeric(n.arms)
  surv.SE <- numeric(n.arms)
  for (i in 1:n.arms) {
    in.arm <- group == arms[i]
    fit    <- survival::survfit(survival::Surv(time[in.arm], status[in.arm]) ~ 1)
    fit.t  <- summary(fit, times = fixedt, extend = TRUE)
    surv[i]    <- fit.t$surv
    surv.SE[i] <- fit.t$std.err
    if (!LOCF && fixedt > max(time[in.arm]) && surv[i] > 0) { # undefined past a censored tail
      surv[i]    <- NA
      surv.SE[i] <- NA
    }
  }
  surv.SE[surv == 0] <- 0 # survival::survfit gives SE = NaN at S = 0
  surv.base    <- surv[1]
  surv.comp    <- surv[-1]
  surv.SE.base <- surv.SE[1]
  surv.SE.comp <- surv.SE[-1]
  diff    <- surv.comp - surv.base
  diff.SE <- sqrt(surv.SE.comp^2 + surv.SE.base^2)
  z       <- stats::qnorm(1 - (1 - conf.level) / 2)
  # Wald chi-square on the cloglog scale
  Wald.chisq <- numeric(n.arms - 1)
  cloglog.mask <- !is.na(diff) & surv.base > 0 & surv.base < 1 & surv.comp > 0 & surv.comp < 1
  var.cloglog.base <- surv.SE.base^2 / (surv.base * log(surv.base))^2
  var.cloglog.comp <- surv.SE.comp[cloglog.mask]^2 / (surv.comp[cloglog.mask] * log(surv.comp[cloglog.mask]))^2
  Wald.chisq[cloglog.mask] <- (log(-log(surv.comp[cloglog.mask])) - log(-log(surv.base)))^2 / (var.cloglog.base + var.cloglog.comp)
  # fallback: identity-scale Wald where either arm is at 0 or 1 (cloglog undefined)
  Wald.chisq[!cloglog.mask] <- diff[!cloglog.mask]^2 / diff.SE[!cloglog.mask]^2
  p <- 2 * stats::pnorm(-sqrt(Wald.chisq))
  p[diff.SE == 0 & diff == 0] <- 1   # fallback: both arms at the same boundary
  p[diff.SE == 0 & diff != 0] <- 0   # fallback: arms at opposite boundaries
  output <- cbind(diff, diff.SE, diff - z * diff.SE, diff + z * diff.SE, p)
  dimnames(output) <- list(arms[-1], c("diff", "SE", "LL", "UL", "p"))
  return(output)
}

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
#' @param time Numeric vector of follow-up times, one per subject.
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
#' data <- data.frame("date.event" = rexp(n, rate = log(2)/6))
#' data$date.censor <- rexp(n, rate = log(2)/12)
#' data$time <- pmin(data$date.event, data$date.censor)
#' data$status <- as.numeric(data$date.event <= data$date.censor)
#' data$date.cycle1 <- 1
#' data$date.cycle1[runif(n) < 0.05] <- NA
#' data$date.cycle1[data$date.cycle1 > data$time] <- NA
#' OR.tdCox(data$time, data$status, X.tdc = data.frame("cycle1" = data$date.cycle1))
#' @family survival
#' @export
OR.tdCox <- function(time, status, X.tdc, X = NULL) {
  # one subject per element of time
  id <- seq_along(time)
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
  formula <- paste0("survival::Surv(tstart, tstop, event) ~ ",
                    paste0(c(colnames(dataC), colnames(dataTDC)), collapse = " + "))
  # cat(formula, "\n")
  fit <- survival::coxph(stats::as.formula(formula), data = data, model = TRUE)
  return(fit)
}
