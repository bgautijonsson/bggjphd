#' Log prior for Theta
#'
#' @param theta the current prior parameters for the latent level parameters
#'
#' @return The log density of the parameter vector Theta
#' @export
#'
#' @examples
pi_theta <- function(theta) {
  mus <- theta[1:4]
  log_prec <- theta[5:8]

  mu_mu <- c(2.5, -1, -0.3, 0.4)
  mu_prec <- c(3, 2, 1, 1)

  out <- 0
  for (i in 1:4) {
    out <- out + dnorm(mus[i], mean = mu_mu[i], sd = 3, log = T)
    out <- out + dnorm(log_prec[i], mean = mu_prec[i], sd = 3, log = T)
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
pi_theta_cond_y <- function(theta, y, x, mu_x, chol_Q_x, Z, chol_Q_e,
                            mu_x_cond_y, chol_Q_x_cond_y) {
  pi_theta(theta) +
    pi_y_cond_x_theta(y = y, Z = Z, x = x, chol_Q_e = chol_Q_e) +
    pi_x_cond_theta(x = x, mu_x = mu_x, chol_Q_x = chol_Q_x) -
    pi_x_cond_theta_y(x = x, mu_x_cond_y = mu_x_cond_y, chol_Q_x_cond_y = chol_Q_x_cond_y)
}

#' Log density of x conditioned on y
#'
#' @param x The current samples of latent parameters x
#' @param mu_x_cond_y The mean of x when conditioning on y
#' @param chol_Q_x_cond_y Cholesky decomposition of precision matrix of x when conditioning on y
#'
#' @return The log density of x when conditioning on y
#' @export
#'
#' @examples
pi_x_cond_theta_y <- function(x, mu_x_cond_y, chol_Q_x_cond_y) {
  sparseMVN::dmvn.sparse(
    x = Matrix::t(x),
    mu = as.vector(mu_x_cond_y),
    CH = chol_Q_x_cond_y,
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
pi_y_cond_x_theta <- function(y, Z, x, chol_Q_e) {
  sparseMVN::dmvn.sparse(
    x = Matrix::t(y),
    mu = as.vector(Z %*% x),
    CH = chol_Q_e,
    prec = TRUE,
    log = TRUE
  )
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
  sparseMVN::dmvn.sparse(
    x = Matrix::t(x),
    mu = as.vector(mu_x),
    CH = chol_Q_x,
    prec = TRUE,
    log = TRUE
  )
}
