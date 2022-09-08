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
met_hast_theta <- function(theta, y, x, mu_x, chol_Q_x, Z, chol_Q_e, mu_x_cond_y, chol_Q_x_cond_y) {
  d <- length(theta)
  mh_sd <-  2.38 * d ^(-1/2)

  mh_samples <- rnorm(n = d, mean = 0, sd = mh_sd)

  theta_prop <- theta + mh_samples

  pi_new <- pi_theta_cond_y(
    theta_prop,
    y,
    x,
    mu_x,
    chol_Q_x,
    Z,
    chol_Q_e,
    mu_x_cond_y,
    chol_Q_x_cond_y
  )

  pi_old <- pi_theta_cond_y(
    theta,
    y,
    x,
    mu_x,
    chol_Q_x,
    Z,
    chol_Q_e,
    mu_x_cond_y, chol_Q_x_cond_y
  )

  if (runif(1) < exp(pi_new - pi_old)) {

    return(theta_prop)

  } else {

    theta

  }
}
