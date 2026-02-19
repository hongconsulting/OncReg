#' Robust linear regression using Huber M-estimation
#'
#' Fits a linear regression model using Huber M-estimation via iteratively 
#' reweighted least squares.
#' @param X numeric design matrix.
#' @param y numeric response vector.
#' @param k2 tuning constant for Huber psi. Default = `1.345`.
#' @param maxit maximum number of iterations. Default = `20`.
#' @param tol convergence tolerance. Default = `0.0001`.
#' @return
#' An object of class `"rlm"` and `"lm"`, broadly compatible with objects 
#' returned by `MASS::rlm()` with minor differences due to rounding.
#' @export
rlm.Huber <- function(X, y, k2 = 1.345, maxit = 20, tol = 0.0001) {
  if (qr(X)$rank < ncol(X)) stop("[rlm.Huber] singularity detected")
  init <- stats::lm.fit(X, y)
  coef <- init$coef
  resid <- init$resid
  scale <- stats::mad(resid, 0)
  done <- FALSE
  trace <- NULL
  for (i in 1:maxit) {
    resid0 <- resid   
    scale <- stats::median(abs(resid))/0.6744897501960817054467
    if (scale == 0) {
      done <- TRUE
      break
    }   
    w <- pmin(1, 1.345/abs(resid/scale))
    fit <- stats::lm.wfit(X, y, w, method = "qr")
    coef <- fit$coefficients
    resid <- fit$residuals
    if (!is.null(resid)) {
      delta <- sqrt(sum((resid0 - resid)^2)/max(0.00000000000000000001, sum(resid0^2)))
    } 
    trace <- c(trace, delta)
    done <- (delta <= tol)
    if (done) break
  }
  if (!done) stop("[rlm.Huber] non-convergence")
  fitted <- drop(X %*% coef)
  call <- match.call()
  call[[1L]] <- as.name("rlm.Huber")
  psi.Huber <- function (x, k = 1.345) {
    return(pmin(1, k/abs(x)))    
  }
  output <- list(coefficients = coef, residuals = y - fitted, 
                 wresid = resid, effects = fit$effects, rank = fit$rank, 
                 fitted.values = fitted, assign = NULL, qr = fit$qr, 
                 df.residual = NA, w = w, s = scale, psi = psi.Huber, k2 = k2, 
                 weights = NULL, conv = trace, 
                 converged = done, x = X, call = call)
  class(output) <- c("rlm", "lm")
  return(output)
}