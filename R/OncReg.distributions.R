#' Generate beta random values from a mean and standard deviation
#'
#' Draws `n` random values from a beta distribution specified by its mean \ifelse{latex}{\out{$\mathit{\mu}$}}{\ifelse{html}{\out{<i>&mu;</i>}}{*mu*}}
#' and standard deviation \ifelse{latex}{\out{$\mathit{\sigma}$}}{\ifelse{html}{\out{<i>&sigma;</i>}}{*sigma*}},
#' rather than its shape parameters, via the method of moments.
#' @param n Number of random values to generate.
#' @param mu Mean \ifelse{latex}{\out{$\mathit{\mu}$}}{\ifelse{html}{\out{<i>&mu;</i>}}{*mu*}}.
#' @param sigma Standard deviation \ifelse{latex}{\out{$\mathit{\sigma}$}}{\ifelse{html}{\out{<i>&sigma;</i>}}{*sigma*}}.
#' @return Numeric vector of length `n`.
#' @family distributions
#' @export
rbeta.mm <- function(n, mu, sigma) {
  kappa <- (mu * (1 - mu))/(sigma^2) - 1
  a <- mu * kappa
  b <- (1 - mu) * kappa
  return(stats::rbeta(n, a, b))
}
