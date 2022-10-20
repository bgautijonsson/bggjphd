#' Perform the second step, smooth, to sample from the posterior
#'
#' @param n_samp The number of samples total (warmup + sampling).
#' @param n_chain The number of chains to sample from.
#' @param fmla
#'
#' @return Results from the Bayesian samples
#' @export
#'
ms_smooth <- function(fmla = station ~ param - 1, theta = NULL, n_samp = 500, n_chain = 4) {

  p <- progressr::progressor(steps = n_chain * n_samp + 1)

  out <- furrr::future_map(
    seq_len(n_chain),
    ~ {
      ms_smooth_sample(
        chain = .x,
        p = p,
        fmla = fmla,
        theta = NULL,
        n_samp = n_samp
      )
    },
    .options = furrr::furrr_options(seed = TRUE, stdout = TRUE)
  )

  p(
    "Converting raw MCMC samples to tidy format",
    class = "sticky"
  )

  tidy_results(out)

}

#' Helper function for performing sampling
#'
#' @param n_samp
#' @param chain
#' @param p
#' @param fmla
#'
#' @return
#' @export
ms_smooth_sample <- function(
    chain,
    p,
    fmla,
    theta,
    n_samp = 500
) {


  p()

  theta <- get_theta()
  priors <- get_priors()

  Z <- make_Z()

  len_eta <- nrow(Z)
  len_nu <- ncol(Z)

  nu <- make_nu(priors)
  eta <- make_eta(Z, nu, theta)
  x <- make_x(nu, eta)


  theta_samp <- matrix(nrow = n_samp, ncol = length(unlist(theta)))
  x_samp <- matrix(nrow = n_samp, ncol = len_eta + len_nu)


  i <- 1

  theta_samp[i, ] <- unlist(theta)



  mu_nu <- make_mu_nu(priors)
  Q_nu <- make_Q_nu(priors)
  Q_e <- make_Q_e(theta_samp[i, ])

  mu_x <- make_mu_x(Z, mu_nu)
  Q_x <- make_Q_x(Q_e, Z, Q_nu)
  chol_Q_x <- Cholesky(Q_x)

  B <- make_B(Z)

  Q_x_cond_etahat <- make_Q_x_cond_etahat(mu_x, Q_x, B)
  mu_x_cond_etahat <- make_mu_x_cond_etahat(mu_x, Q_x, Q_x_cond_etahat, B)
  chol_Q_x_cond_etahat <- Cholesky(Q_x_cond_etahat)


  x_samp[i, ] <- sample_pi_x_cond_etahat(mu_x_cond_etahat, chol_Q_x_cond_etahat)
  x <- x_samp[i, ]
  eta <- x[1:len_eta]
  nu <- x[-(1:len_eta)]


  while (i < n_samp) {
    i <- i + 1
    p()


    theta_prop <- propose_theta(theta_samp[i - 1, ])

    mu_nu_prop <- make_mu_nu(priors)
    Q_nu_prop <- make_Q_nu(priors)
    Q_e_prop <- make_Q_e(theta_prop)

    mu_x_prop <- make_mu_x(Z, mu_nu_prop)
    Q_x_prop <- make_Q_x(Q_e_prop, Z, Q_nu_prop)
    chol_Q_x_prop <- Cholesky(Q_x_prop)

    Q_x_cond_etahat_prop <- make_Q_x_cond_etahat(mu_x_prop, Q_x_prop, B)
    mu_x_cond_etahat_prop <- make_mu_x_cond_etahat(mu_x_prop, Q_x_prop, Q_x_cond_etahat_prop, B)
    chol_Q_x_cond_etahat_prop <- Cholesky(Q_x_cond_etahat_prop)


    pi_new <- pi_theta_cond_etahat(
      theta_prop,
      eta,
      x, mu_x_prop, chol_Q_x_prop,
      mu_x_cond_etahat_prop, chol_Q_x_cond_etahat_prop
    )

    pi_old <- pi_theta_cond_etahat(
      theta_samp[i - 1, ],
      eta,
      x, mu_x, chol_Q_x,
      mu_x_cond_etahat, chol_Q_x_cond_etahat
    )

    if (exp(pi_new - pi_old) > runif(1)) {

      theta_samp[i, ] <- theta_prop
      mu_nu <- mu_nu_prop
      Q_nu <- Q_nu_prop
      Q_e <- Q_e_prop

      mu_x <- mu_x_prop
      Q_x <- Q_x_prop
      chol_Q_x <- chol_Q_x_prop

      Q_x_cond_etahat <- Q_x_cond_etahat_prop
      mu_x_cond_etahat <- mu_x_cond_etahat_prop
      chol_Q_x_cond_etahat <- chol_Q_x_cond_etahat_prop



    } else {

      theta_samp[i, ] <- theta_samp[i - 1, ]

    }

    x_samp[i, ] <- sample_pi_x_cond_etahat(mu_x_cond_etahat, chol_Q_x_cond_etahat)
    x <- x_samp[i, ]
    eta <- x[1:len_eta]
    nu <- x[-(1:len_eta)]

  }



  list(
    "theta" = theta_samp,
    "x" = x_samp
  )

}
