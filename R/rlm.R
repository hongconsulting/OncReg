#' Robust Akaike information criterion for Huber M-estimation
#'
#' Computes Ronchetti's robust analogue¹ of the Akaike information criterion²
#' (AICR) for a linear model fitted by Huber M-estimation³. The effective
#' degrees of freedom term is evaluated using the the Moore–Penrose
#' pseudoinverse⁴ for numerical stability under rank deficiency.
#' @param X numeric design matrix.
#' @param y numeric response vector.
#' @param beta numeric vector of regression coefficients from a Huber
#' M-estimation fit.
#' @param scale scale estimate from a Huber M-estimation fit.
#' @param k2 tuning constant for the Huber \ifelse{latex}{\out{$\mathit{\psi}$}}{\ifelse{html}{\out{<i>&psi;</i>}}{*psi*}}-function. Default = `1.345`.
#' @return The AICR value.
#' @references
#' 1. Ronchetti, E., 1985. Robust model selection in regression. *Statistics &
#' Probability Letters*, 3(1), pp. 21–23.
#' 2. Akaike, H., 1974. A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control*, 19(6), pp. 716–723.
#' 3. Huber, P.J., 1973. Robust regression: asymptotics, conjectures and Monte
#' Carlo. *The Annals of Statistics*, pp. 799–821.
#' 4. Penrose, R., 1955. A generalized inverse for matrices. In: *Mathematical
#' Proceedings of the Cambridge Philosophical Society*, 51(3), pp. 406–413.
#' Cambridge: Cambridge University Press.
#' @examples
#' y <- c(36.3, 47.9, 47.2, 43.9, 47.6, 49.6, 53.2, 59.3, 63.2, 70.8, 75.9, 88.5,
#'        97.3, 103.6, 6.1, 120.2, 135.8, 139.4)
#' x <- as.matrix(1:length(y) - 1)
#' fit <- rlm.Huber(X = x, y = y)
#' print(AICR.Huber(x, y, fit$coefficients, fit$s))
#' @export
AICR.Huber <- function(X, y, beta, scale, k2 = 1.345) {
  X <- as.matrix(X)
  beta <- as.numeric(beta)
  y <- as.numeric(y)
  n <- length(y)
  r <- (y - drop(X %*% beta)) / scale
  dpsi  <- dscore.Huber(r, k2)
  psi_sq <- score.Huber(r, k2)^2
  X_dpsi  <- X * dpsi
  X_psi_sq <- X * psi_sq
  J <- crossprod(X_dpsi, X)  / (n * scale^2)
  K <- crossprod(X_psi_sq, X) / (n * scale^2)
  S <- svd(J)
  tol <- max(dim(J)) * max(S$d) * .Machine$double.eps
  d_inv <- rep(0, length(S$d))
  i <- S$d > tol
  d_inv[i] <- 1 / S$d[i]
  J_pseudoinv <- S$v %*% (d_inv * t(S$u))
  return(2 * n * log(scale) + 2 * sum(diag(J_pseudoinv %*% K)))
}

dscore.Huber <- function(x, k2) {
  output <- numeric(length(x))
  output[abs(x) <= k2] <- 2
  return(output)
}

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
  coef <- init$coefficients
  resid <- init$residuals
  fit <- init
  w <- rep(1, length(y))
  trace <- NULL
  done <- FALSE
  for (i in 1:maxit) {
    resid0 <- resid
    scale <- stats::median(abs(resid)) / 0.6744897501960817054467
    if (scale == 0) {
      done <- TRUE
      break
    }
    w <- pmin(1, k2 / abs(resid / scale))
    fit <- stats::lm.wfit(X, y, w, method = "qr")
    coef <- fit$coefficients
    resid <- fit$residuals
    delta <- sqrt(sum((resid0 - resid)^2) / max(1e-20, sum(resid0^2)))
    trace <- c(trace, delta)
    if (delta <= tol) {
      done <- TRUE
      break
    }
  }
  if (!done) {
    message("[rlm.Huber]")
    message("resid0:")
    print(resid0)
    message("resid:")
    print(resid)
    message("delta:")
    print(delta)
    stop("[rlm.Huber] non-convergence")
  }
  fitted <- drop(X %*% coef)
  weight.Huber <- function(x, k = k2) {
    return(pmin(1, k / abs(x)))
  }
  call <- match.call()
  call[[1L]] <- as.name("rlm.Huber")
  output <- list(coefficients = coef, residuals = y - fitted, wresid = resid,
                 effects = fit$effects, rank = fit$rank, fitted.values = fitted,
                 assign = NULL, qr = fit$qr, df.residual = NA, w = w, s = scale,
                 psi = weight.Huber, k2 = k2, weights = NULL, conv = trace,
                 converged = done, x = X, call = call)
  class(output) <- c("lm", "rlm")
  return(output)
}

rlm.Huber.univarpoly.AICR <- function(x, y, max.degree = 3, p = 0.05, tol = 0.0001) {
  n <- length(y)
  max.degree <- min(n - 2, max.degree)
  X <- outer(x, 0:max.degree, "^")
  X0 <- as.matrix(X[, 1])
  fit <- rlm.Huber(X = X0, y = y, tol = tol)
  AICR <- AICR.Huber(X = X0, y = y, beta = fit$coefficients, scale = fit$s)
  for (i in 1:max.degree) {
    X1 <- as.matrix(X[, 1:(i + 1)])
    fit1 <- rlm.Huber(X = X1, y = y, tol = tol)
    AICR1 <- AICR.Huber(X = X1, y = y, beta = fit1$coefficients, scale = fit1$s)
    if (AICR1 < AICR) {
      fit <- fit1
      AICR <- AICR1
    }
  }
  return(fit)
}

score.Huber <- function(x, k2) {
  output <- 2 * k2 * sign(x)
  i <- abs(x) <= k2
  output[i] <- 2 * x[i]
  return(output)
}

