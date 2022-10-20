#' Metropolis Sampler for Theta
#'
#' @param theta The current value of theta
#' @param y The current value of y
#' @param x The current value of x
#' @param mu_x The mean vector for x
#' @param chol_Q_x Cholesky decomposition of precision matrix of x
#' @param Z Model matrix for y as in y = Z * x + e
#' @param chol_Q_e Cholesky decomposition of banded diagonal matrix containing the Hessian matrices for each station
#'   from the Max step
#' @param mu_x_cond_y Mean vector for x given y
#' @param chol_Q_x_cond_y Cholesky decomposition of precision matrix of x given y
#'
#' @return Samples for theta given the previous values of theta
#' @export
#'
#' @examples
met_hast_theta <- function(
    theta,
    Z, nu,
    x, mu_x, chol_Q_x,
    mu_x_cond_etahat, chol_Q_x_cond_etahat
) {
  d <- length(theta)
  mh_sd <-  2.3 * d ^(-1/2)

  mh_samples <- rnorm(n = d, mean = 0, sd = mh_sd)

  theta_prop <- theta + mh_samples

  pi_new <- pi_theta_cond_etahat(
    theta_prop,
    eta,
    x, mu_x, chol_Q_x,
    mu_x_cond_etahat, chol_Q_x_cond_etahat
  )

  pi_old <- pi_theta_cond_etahat(
    theta,
    eta,
    x, mu_x, chol_Q_x,
    mu_x_cond_etahat, chol_Q_x_cond_etahat
  )

  if (exp(pi_new - pi_old) > runif(1)) {

    return(theta_prop)

  } else {

    theta

  }
}


#' Title
#'
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
propose_theta <- function(theta) {
  d <- length(theta)
  mh_sd <-  2.4 * d ^(-1/2)

  mh_samples <- rnorm(n = d, mean = 0, sd = mh_sd)

  theta_prop <- theta + mh_samples

  theta_prop
}
