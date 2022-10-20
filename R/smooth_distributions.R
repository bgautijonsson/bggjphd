#' Log prior for Theta
#'
#' @param theta the current prior parameters for the latent level parameters
#'
#' @return The log density of the parameter vector Theta
#' @export
#'
#' @examples
pi_theta <- function(theta) {
  log_sqrt_prec_e <- theta[1:4]
  out <- 0
  for (i in 1:4) {
    out <- out + dnorm(log_sqrt_prec_e[i], mean = 2, sd = 4, log = T)
  }

  out
}


#' The log density of Theta conditioned on all other parameters
#'
#' @param theta The vector containing the prior parameters for latent parameters in x
#' @param y The current samples for estimating the outcome values from the Max step
#' @param x The current estimates of the latent level parameters
#' @param mu_x The first four parameters in Theta
#' @param chol_Q_x Cholesky decomposition of the latter four parameters in Theta
#' @param Z Model matrix as in Y = Z * X + e
#' @param chol_Q_e Cholesky decomposition of block diagonal matrix containing the hessians
#'   for each station from the Max step
#' @param mu_x_cond_y Same as mu_x except after conditioning on the values of Y
#' @param chol_Q_x_cond_y Cholesky decomposition of conditional precision of x after conditioning on y
#'
#' @return The log density of Theta given all other parameters
#' @export
#'
#' @examples
pi_theta_cond_etahat <- function(
    theta,
    eta,
    x, mu_x, chol_Q_x,
    mu_x_cond_etahat, chol_Q_x_cond_etahat
) {
  pi_theta(theta) +
    pi_etahat_cond_x_theta(eta) +
    pi_x_cond_theta(x, mu_x, chol_Q_x) -
    pi_x_cond_theta_etahat(x, mu_x_cond_etahat, chol_Q_x_cond_etahat)
}


#' The log density of x conditioned on theta
#'
#' @param x Current values for the latent parameters x
#' @param mu_x Mean vector for x
#' @param chol_Q_x Cholesky decomposition of the precision matrix for x
#'
#' @return The log density of x conditioned on theta
#' @export
#'
#' @examples
pi_x_cond_theta <- function(x, mu_x, chol_Q_x) {
  t <- Matrix::t
  dmvn.sparse(
    x = t(x),
    mu = as.vector(mu_x),
    CH = chol_Q_x,
    prec = TRUE,
    log = TRUE
  )
}

#' Log density of x conditioned on etahat and theta
#'
#' @param mu_x_cond_etahat
#' @param chol_Q_x_cond_etahat
#' @param x The current samples of latent parameters x
#'
#' @return The log density of x when conditioning on y
#' @export
#'
#' @examples
pi_x_cond_theta_etahat <- function(x, mu_x_cond_etahat, chol_Q_x_cond_etahat) {
  t <- Matrix::t
  dmvn.sparse(
    x = t(x),
    mu = as.vector(mu_x_cond_etahat),
    CH = chol_Q_x_cond_etahat,
    prec = TRUE,
    log = TRUE
  )
}




#' Log density of y given x and theta
#'
#' @param y The current samples of y
#' @param Z The model matrix as in y = Z * x + e
#' @param x The current samples of the latent parameters x
#' @param chol_Q_e Block diagonal matrix of Hessian matrices from the Max step
#'
#' @return The log density of y conditioned on x and theta
#' @export
#'
#' @examples
pi_etahat_cond_x_theta <- function(eta) {
  t <- Matrix::t
  dmvn.sparse(
    x = t(eta_hat),
    mu = as.vector(eta),
    CH = chol_Q_etay,
    prec = TRUE,
    log = TRUE
  )
}

#' Title
#'
#' @param mu_x_cond_etahat
#' @param chol_Q_x_cond_etahat
#'
#' @return
sample_pi_x_cond_etahat <- function(mu_x_cond_etahat, chol_Q_x_cond_etahat) {
  sparseMVN::rmvn.sparse(
    n = 1,
    mu = mu_x_cond_etahat,
    CH = chol_Q_x_cond_etahat,
    prec = TRUE
  ) |>
    as.numeric()
}


