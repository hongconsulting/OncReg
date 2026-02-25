#' Robust linear regression using Huber M-estimation
#'
#' Fits a linear regression model using Huber M-estimation¹ via iteratively
#' reweighted least squares.
#' @param X numeric design matrix.
#' @param y numeric response vector.
#' @param k2 tuning constant for Huber \ifelse{latex}{\out{$\mathit{\psi}$}}{\ifelse{html}{\out{<i>&psi;</i>}}{*psi*}}-function. Default = `1.345`.
#' @param maxit maximum number of iterations. Default = `100`.
#' @param tol convergence tolerance. Default = `0.0001`.
#' @return
#' An object of classes `"lm"` and `"rlm"`, broadly compatible with objects
#' returned by `MASS::rlm()` with minor differences due to rounding.
#' @references
#' 1. Huber, P.J., 1973. Robust regression: asymptotics, conjectures and Monte
#' Carlo. *The Annals of Statistics*, pp. 799–821.
#' @export
rlm.Huber <- function(X, y, k2 = 1.345, maxit = 100, tol = 0.0001) {
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
    w <- pmin(1, k2/abs(resid/scale))
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
  weight.Huber <- function (x, k = k2) {
    return(pmin(1, k/abs(x)))
  }
  output <- list(coefficients = coef, residuals = y - fitted,
                 wresid = resid, effects = fit$effects, rank = fit$rank,
                 fitted.values = fitted, assign = NULL, qr = fit$qr,
                 df.residual = NA, w = w, s = scale, psi = weight.Huber, k2 = k2,
                 weights = NULL, conv = trace,
                 converged = done, x = X, call = call)
  class(output) <- c("lm", "rlm")
  return(output)
}

#' Robust Akaike information criterion for Huber M-estimation
#'
#' Computes Ronchetti's robust analogue¹ of the Akaike information criterion²
#' (AICR) for a linear model fitted by Huber M-estimation³.
#' @param X numeric design matrix.
#' @param y numeric response vector.
#' @param beta numeric vector of regression coefficients from a Huber
#' M-estimation fit.
#' @param scale scale estimate from the fit from a Huber M-estimation fit.
#' @param k2 tuning constant for the Huber \ifelse{latex}{\out{$\mathit{\psi}$}}{\ifelse{html}{\out{<i>&psi;</i>}}{*psi*}}-function. Default = `1.345`.
#' @return The robust AIC value.
#' @references
#' 1. Ronchetti, E., 1985. Robust model selection in regression. *Statistics &
#' Probability Letters*, 3(1), pp. 21–23.
#' 2. Akaike, H., 1974. A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control*, 19(6), pp. 716–723.
#' 3. Huber, P.J., 1973. Robust regression: asymptotics, conjectures and Monte
#' Carlo. *The Annals of Statistics*, pp. 799–821.
#' @export
AICR.Huber <- function(y, X, beta, scale, k2 = 1.345) {
  X <- as.matrix(X)
  beta <- as.numeric(beta)
  y <- as.numeric(y)
  n <- length(y)
  r <- (y - drop(X %*% beta)) / scale
  dpsi  <- dscore.Huber(r, k2)
  psisq <- score.Huber(r, k2)^2
  X_dpsi  <- X * dpsi
  X_psisq <- X * psisq
  J <- crossprod(X_dpsi, X)  / (n * scale^2)
  K <- crossprod(X_psisq, X) / (n * scale^2)
  invJ <- solve(J)
  return(2 * n * log(scale) + 2 * sum(diag(invJ %*% K)))
}

score.Huber <- function(x, k2) {
  output <- 2 * k2 * sign(x)
  i <- abs(x) <= k2
  output[i] <- 2 * x[i]
  return(output)
}

dscore.Huber <- function(x, k2) {
  output <- numeric(length(x))
  output[abs(x) <= k2] <- 2
  return(output)
}
