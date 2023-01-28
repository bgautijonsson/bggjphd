#' Title
#'
#' @param n_samp
#' @param type
#'
#' @return
init_params <- function(n_samp = 500, type = "spatial") {
  if (type == "basic") {
    params <- init_params_basic(n_samp = n_samp)
  } else {
    params <- init_params_spatial(n_samp = n_samp)
  }
}

#' Title
#'
#' @param params
#' @param type
#'
#' @return
update_theta <- function(params, type = "spatial") {
  if (type == "basic") {
    params <- update_theta_basic(params)
  } else if (type == "spatial") {
    params <- update_theta_spatial(params)
  }

  params
}


#' Title
#'
#' @return
init_params_basic <- function(n_samp = 500) {

  theta <- init_theta()
  priors <- get_priors()

  Z <- make_Z()


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
    "priors" = priors,
    "accept" = 0.44,
    "iter" = 1,
    "n_samp" = n_samp,
    "mult" = 2.3
  )
}

#' A function to perform a metropolis hastings step on the proposed theta vector
#'
#' @param params
#' @param theta_prop
#'
#' @return
update_theta_basic <- function(params) {
  params$iter <- params$iter + 1

  params <- propose_theta(params)
  theta_prop <- params$theta_prop

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

  accepted <- 0

  if (pi_new - pi_old > log(runif(1))) {
    params$Q_e <- Q_e_prop
    params$Q_x <- Q_x_prop
    params$chol_Q_x <- chol_Q_x_prop

    params$Q_x_cond_etahat <- Q_x_cond_etahat_prop
    params$mu_x_cond_etahat <- mu_x_cond_etahat_prop
    params$chol_Q_x_cond_etahat <- chol_Q_x_cond_etahat_prop

    params$theta <- theta_prop

    accepted <- accepted + 1

  }

  params$x <- sample_pi_x_cond_etahat(params$mu_x_cond_etahat, params$chol_Q_x_cond_etahat)
  params$eta <- Matrix(
    params$x[seq_len(nrow(params$eta))],
    ncol = 1
  )
  params$nu <- Matrix(
    params$x[-seq_len(nrow(params$eta))],
    ncol = 1
  )

  params$accept <- (99*params$accept + accepted)/100

  params

}


#' Title
#'
#' @return
init_params_spatial <- function(n_samp = n_samp) {

  theta <- init_theta()

  x <- init_eta_spatial(theta)
  Q_e <- make_Q_e_spatial(theta)
  # chol_Q_e <- Cholesky(Q_e)

  eta_zero <- Matrix::Matrix(
    0,
    ncol = 1,
    nrow = nrow(x)
  )


  Q_eta_cond_etahat <- make_Q_eta_cond_etahat_spatial(Q_e)
  mu_eta_cond_etahat <- make_mu_eta_cond_etahat_spatial(Q_eta_cond_etahat)
  chol_Q_eta_cond_etahat <- Matrix::Cholesky(Q_eta_cond_etahat)


  list(
    "x" = x,
    "Q_e" = Q_e,
    "eta_zero" = eta_zero,
    "Q_eta_cond_etahat" = Q_eta_cond_etahat,
    "mu_eta_cond_etahat" = mu_eta_cond_etahat,
    "chol_Q_eta_cond_etahat" = chol_Q_eta_cond_etahat,
    "theta" = theta,
    "accept" = 0.44,
    "iter" = 1,
    "n_samp" = n_samp,
    "mult" = 2.3,
    "mean" = rep(0, 4),
    "variance" = rep(1, 4)
  )
}

#' A function to perform a metropolis-hastings step on the proposed theta vector
#'
#' @param params
#' @param theta_prop
#'
#' @return
update_theta_spatial <- function(params) {

  params$iter <- params$iter + 1

  params <- propose_theta(params)
  theta_prop <- params$theta_prop

  Q_e_prop <- make_Q_e_spatial(theta_prop)

  Q_eta_cond_etahat_prop <- make_Q_eta_cond_etahat_spatial(Q_e_prop)
  mu_eta_cond_etahat_prop <- make_mu_eta_cond_etahat_spatial(Q_eta_cond_etahat_prop)
  chol_Q_eta_cond_etahat_prop <- Matrix::Cholesky(Q_eta_cond_etahat_prop)

  pi_new <- pi_theta_cond_etahat_spatial(
    theta_prop,
    params$eta_zero,
    chol_Q_eta_cond_etahat_prop, mu_eta_cond_etahat_prop
  )

  pi_old <- pi_theta_cond_etahat_spatial(
    params$theta,
    params$eta_zero,
    params$chol_Q_eta_cond_etahat, params$mu_eta_cond_etahat
  )

  accepted <- 0

  if (pi_new - pi_old > log(runif(1))) {
    params$Q_e <- Q_e_prop
    # params$chol_Q_e <- chol_Q_e_prop

    params$Q_eta_cond_etahat <- Q_eta_cond_etahat_prop
    params$mu_eta_cond_etahat <- mu_eta_cond_etahat_prop
    params$chol_Q_eta_cond_etahat <- chol_Q_eta_cond_etahat_prop

    params$theta <- theta_prop

    accepted <- accepted + 1

  }

  params$x <- sample_pi_eta_cond_etahat_spatial(params$chol_Q_eta_cond_etahat, params$mu_eta_cond_etahat)


  params$accept <- (99*params$accept + accepted)/100

  if (params$iter < params$n_samp / 2) {
    params$mean <- (99*params$mean + params$theta)/100
    params$variance <- (99*params$variance + (params$theta - params$mean)^2) / 100
  }

  params

}
