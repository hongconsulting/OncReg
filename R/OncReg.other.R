#' Collapse repeated measurements across IDs
#'
#' For each ID in a data frame, collapses all rows belonging to that ID into a
#' single row. Variables whose names match a specified pattern are summarized
#' according to a chosen `method`. The output data frame contains one row per
#' unique ID and one column per matched variable. Non-matching variables are
#' ignored.
#' @param data A data frame containing repeated measurements.
#' @param ID_varname Character string giving the name of the ID variable. Rows
#' with the same ID are treated as belonging to the same individual.
#' @param pattern String containing a regular expression used to select
#' variables. All variable names matching this pattern (via `grep()`) are
#' collapsed.
#' @param method Character string specifying how multiple values within each ID
#' should be collapsed. Options include:
#' \itemize{
#'   \item `"left"`: Returns the first non-missing value in order of appearance.
#'   \item `"mean"`: Returns the mean of non-missing values.
#'   \item `"max"`: Returns the maximum of non-missing values.
#'   \item `"min"`: Returns the minimum of non-missing values.
#'   \item `"single"` (default): Returns the mean of non-missing values and prints
#'   a message if more than one unique non-missing value is found.
#'   \item `"sum"`: Returns the sum of non-missing values.
#' }
#' @details
#' An example use case is when working with REDCap exports, where the
#' measurements for one patient are distributed across multiple event rows.
#' @return A data frame with one row per unique ID and one column for each
#' variable matching `pattern`, summarized according to `method`.
#' @family other
#' @export
OR.collapse <- function(data, ID_varname, pattern, method = "single") {
  IDs  <- unique(data[[ID_varname]])
  varnames <- grep(pattern, names(data), value = TRUE)
  output <- data.frame("ID" = IDs)
  for (i in 1:length(varnames)) {
    varname <- varnames[i]
    output[[varname]] <- NA
    for (j in 1:length(IDs)) {
      ID <- IDs[j]
      ID_mask <- data[[ID_varname]] == ID
      subdata <- data[[varname]][ID_mask]
      subdata_na_omit <- OR.NA.rm(subdata)
      if (method != "single" & length(subdata_na_omit) == 0) {
        message("[OR.collapse] ", varname, " for ID ", ID, " is empty")
        next
      }
      subdata_unique <- unique(subdata_na_omit)
      if (method == "left") {
        output[[varname]][j] <- subdata_na_omit[1]
      } else if (method == "max") {
        output[[varname]][j] <- max(subdata_unique)
      } else if (method == "mean" | method == "single") {
        if (method == "single" & length(subdata_unique) > 1) {
          message("[OR.collapse] ", varname, " for ID ", ID, " has more than 1 unique value\n")
        }
        output[[varname]][j] <- mean(subdata_na_omit)
      } else if (method == "min") {
        output[[varname]][j] <- min(subdata_unique)
      } else if (method == "sum") {
        output[[varname]][j] <- sum(subdata_na_omit)
      } else {
        stop(paste0("[OR.collapse] method ", method, " not implemented"))
      }
    }
  }
  return(output)
}

#' Random hold-out allocation
#'
#' Randomly allocates `n` observations to a hold-out set of size
#' `ceiling(n * proportion)`, with the remainder allocated to the training set.
#' The number of hold-out observations is fixed by construction rather than
#' binomially distributed and the output is reproducible for a given `seed`.
#' @param n Integer giving the number of observations to allocate.
#' @param proportion Numeric giving the proportion of observations allocated to
#' the hold-out set, rounded up to the nearest whole observation.
#' @param seed Integer seed for random number generation. Default = `24601`.
#' @return Numeric vector of length `n` containing `1` for hold-out observations
#' and `0` for training observations.
#' @family other
#' @export
OR.holdout <- function(n, proportion, seed = 24601) {
  set.seed(seed)
  k <- ceiling(n * proportion)
  return(sample(rep(c(1, 0), c(k, n - k))))
}

#' Enumerate permutations of k out of n items
#'
#' Returns all length-`k` permutations of the integers from 1 to `n`, in
#' lexicographic order.
#' @param n Integer size of the set.
#' @param k Integer number of elements per permutation.
#' @return Integer matrix with `factorial(n)/factorial(n-k)` rows and `k` columns.
#' @family other
#' @export
OR.permutations <- function(n, k) {
  if (k > n) stop("k > n")
  total <- factorial(n) / factorial(n - k)
  output <- matrix(0, total, k)
  a <- seq_len(n)
  output[1, ] <- a[1:k]
  i <- 1
  while (TRUE) {
    j <- n - 1
    while (j > 0 && a[j] > a[j + 1]) j <- j - 1
    if (j == 0) break
    l <- n
    while (a[j] > a[l]) l <- l - 1
    temp <- a[j]; a[j] <- a[l]; a[l] <- temp
    a[(j + 1):n] <- rev(a[(j + 1):n])
    i <- i + 1
    output[i, ] <- a[1:k]
  }
  return(output)
}

#' Read CSV files matching a path stub and combine by rows
#'
#' Reads all comma-separated values (CSV) files whose paths begin with `path.stub`
#' and combines them by rows.
#' @param path.stub String path prefix used to match CSV files.
#' @param ... Additional arguments passed to `read.csv()`.
#' @return A `data.frame` containing contents of all matched CSV files combined
#' by rows.
#' @family other
#' @export
OR.read.csv.stub <- function(path.stub, ...) {
  files <- Sys.glob(paste0(path.stub, "*.csv"))
  output <- do.call(rbind, lapply(files, utils::read.csv, ...))
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
#' @family other
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
  fit <- survival::coxph(stats::as.formula(formula), data = data)
  return(fit)
}
