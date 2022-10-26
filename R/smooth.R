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

  params <- init_params()
  len_eta <- nrow(params$Z)
  len_nu <- ncol(params$Z)


  theta_samp <- matrix(nrow = n_samp, ncol = length(params$theta))
  x_samp <- matrix(nrow = n_samp, ncol = len_eta + len_nu)


  i <- 1

  theta_samp[i, ] <- params$theta


  x_samp[i, ] <- sample_pi_x_cond_etahat(params$mu_x_cond_etahat, params$chol_Q_x_cond_etahat)

  params$x <- x_samp[i, ]
  params$eta <- params$x[1:len_eta]
  params$nu <- params$x[-(1:len_eta)]


  while (i < n_samp) {
    i <- i + 1
    p()



    params <- update_theta(params)

    theta_samp[i, ] <- params$theta

    x_samp[i, ] <- sample_pi_x_cond_etahat(params$mu_x_cond_etahat, params$chol_Q_x_cond_etahat)
    params$x <- x_samp[i, ]
    params$eta <- params$x[1:len_eta]
    params$nu <- params$x[-(1:len_eta)]

  }



  list(
    "theta" = theta_samp,
    "x" = x_samp,
    "chain" = chain
  )

}
