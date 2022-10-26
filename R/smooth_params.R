#' Title
#'
#' @return
#'
#' @examples
init_params <- function() {

  theta <- init_theta()
  priors <- get_priors()

  Z <- make_Z()

  len_eta <- nrow(Z)
  len_nu <- ncol(Z)

  nu <- init_nu(priors)
  eta <- init_eta(Z, nu, theta)
  Q_e <- make_Q_e(theta)
  eta_zero <- Matrix(
    0,
    ncol = 1,
    nrow = nrow(eta)
  )

  mu_nu <- make_mu_nu(priors)
  Q_nu <- make_Q_nu(priors)

  mu_x <- make_mu_x(Z, mu_nu)
  Q_x <- make_Q_x(Q_e, Z, Q_nu)
  chol_Q_x <- Cholesky(Q_x)

  x <- make_x(nu, eta)
  x_zero <- Matrix(
    0,
    ncol = 1, nrow = nrow(x)
  )

  B <- make_B(Z)

  Q_x_cond_etahat <- make_Q_x_cond_etahat(mu_x, Q_x, B)
  mu_x_cond_etahat <- make_mu_x_cond_etahat(mu_x, Q_x, Q_x_cond_etahat, B)
  chol_Q_x_cond_etahat <- Cholesky(Q_x_cond_etahat)


  list(
    "Z" = Z,
    "B" = B,
    "eta" = eta,
    "Q_e" = Q_e,
    "eta_zero" = eta_zero,
    "nu" = nu,
    "mu_nu" = mu_nu,
    "Q_nu" = Q_nu,
    "x" = x,
    "x_zero" = x_zero,
    "mu_x" = mu_x,
    "Q_x" = Q_x,
    "chol_Q_x" = chol_Q_x,
    "Q_x_cond_etahat" = Q_x_cond_etahat,
    "mu_x_cond_etahat" = mu_x_cond_etahat,
    "chol_Q_x_cond_etahat" = chol_Q_x_cond_etahat,
    "theta" = theta,
    "priors" = priors
  )
}

#' A function to perform a metropolis hastings step on the proposed theta vector
#'
#' @param params
#' @param theta_prop
#'
#' @return
#'
#' @examples
update_theta <- function(params) {

  theta_prop <- propose_theta(params$theta)

  Q_e_prop <- make_Q_e(theta_prop)
  Q_x_prop <- make_Q_x(Q_e_prop, params$Z, params$Q_nu)
  chol_Q_x_prop <- Cholesky(Q_x_prop)

  Q_x_cond_etahat_prop <- make_Q_x_cond_etahat(params$mu_x, Q_x_prop, params$B)
  mu_x_cond_etahat_prop <- make_mu_x_cond_etahat(params$mu_x, Q_x_prop, Q_x_cond_etahat_prop, params$B)
  chol_Q_x_cond_etahat_prop <- Cholesky(Q_x_cond_etahat_prop)



  pi_new <- pi_theta_cond_etahat(
    theta_prop,
    params$eta_zero,
    params$x_zero, params$mu_x, chol_Q_x_prop,
    mu_x_cond_etahat_prop, chol_Q_x_cond_etahat_prop
  )

  pi_old <- pi_theta_cond_etahat(
    params$theta,
    params$eta_zero,
    params$x_zero, params$mu_x, params$chol_Q_x,
    params$mu_x_cond_etahat, params$chol_Q_x_cond_etahat
  )

  if (pi_new - pi_old > log(runif(1))) {
    params$Q_e <- Q_e_prop
    params$Q_x <- Q_x_prop
    params$chol_Q_x <- chol_Q_x_prop

    params$Q_x_cond_etahat <- Q_x_cond_etahat_prop
    params$mu_x_cond_etahat <- mu_x_cond_etahat_prop
    params$chol_Q_x_cond_etahat <- chol_Q_x_cond_etahat_prop

    params$theta <- theta_prop

  }

  params

}
