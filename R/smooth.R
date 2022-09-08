#' Title
#'
#' @param fit_dat
#' @param n_samp
#' @param n_chain
#'
#' @return
#' @export
#'
#' @examples
ms_smooth <- function(fit_dat, n_samp = 500, n_chain = 4) {
  require(Matrix)
  priors <- get_priors(fit_dat)

  eta_hat <- make_eta_hat(fit_dat)
  Q_e <- make_Q_e(fit_dat)
  chol_Q_e <- Matrix::Cholesky(Q_e)

  Z <- make_Z(eta_hat)
  nu <- make_nu()

  y <- eta_hat
  x <- nu

  len_y <- nrow(y)
  len_x <- nrow(x)


  p <- progressr::progressor(steps = n_chain * n_samp)
  out <- furrr::future_map(
    seq_len(n_chain),
    ~ {
      ms_smooth_sample(
        chain = .x,
        p = p,
        y = y,
        len_y = len_y,
        x = x,
        len_x = len_x,
        Q_e = Q_e,
        chol_Q_e = chol_Q_e,
        eta_hat = eta_hat,
        Z = Z,
        priors = priors,
        n_samp = n_samp
      )
    },
    .options = furrr::furrr_options(seed = TRUE, stdout = TRUE)
  )

  samples <- tibble()

  for (i in 1:n_chain) {
    samples <- samples |>
      bind_rows(
        out[[i]]$theta |>
          as.data.frame() |>
          as_tibble() |>
          set_names(c("mu_psi", "mu_tau", "mu_phi", "mu_xi", "prec_psi", "prec_tau", "prec_phi", "prec_xi")) |>
          mutate(iter = row_number(),
                 chain = i)
      )

  }

  samples


}

#' Title
#'
#' @param y
#' @param x
#' @param Q_e
#' @param chol_Q_e
#' @param eta_hat
#' @param Z
#' @param priors
#' @param n_samp
#'
#' @return
#' @export
#'
#' @examples
ms_smooth_sample <- function(chain, p, y, len_y, x, len_x, Q_e, chol_Q_e, eta_hat, Z, priors, n_samp = 500) {

  # p <- progressr::progressor(steps = n_samp)

  p(sprintf("Chain: %g", chain))

  theta <- rnorm(8, mean = priors, sd = 4)
  theta_samp <- matrix(nrow = n_samp, ncol = length(theta))

  yx_samp <- matrix(nrow = n_samp, ncol = len_y + len_x)

  i <- 1

  theta_samp[i, ] <- theta

  mu_x <- Matrix::Matrix(theta_samp[i, 1:4])
  Q_x <- Matrix::.sparseDiagonal(n = nrow(mu_x), x = exp(theta_samp[i, 5:8])^2)
  chol_Q_x <- Matrix::Cholesky(Q_x)


  Q_x_cond_y <- Q_x + Matrix::t(Z) %*% Q_e %*% Z
  chol_Q_x_cond_y <- Matrix::Cholesky(Q_x_cond_y)

  mu_x_cond_y <- Matrix::solve(Q_x_cond_y, Q_x %*% mu_x + Matrix::t(Z) %*% Q_e %*% y)


  mu_yx_cond_theta <- rbind(Z %*% mu_x_cond_y, mu_x_cond_y)

  Q_yx_cond_theta <- Matrix::Matrix(
    data = 0,
    nrow = nrow(mu_yx_cond_theta),
    ncol = nrow(mu_yx_cond_theta)
  )

  # Upper left of matrix
  Q_yx_cond_theta[1:len_y, 1:len_y] <- Q_e
  # Upper right of matrix
  Q_yx_cond_theta[1:len_y, (len_y + 1):(len_y + len_x)] <- -Q_e %*% Z
  # Lower left of matrix
  Q_yx_cond_theta[(len_y + 1):(len_y + len_x), 1:len_y] <- - Matrix::t(Z) %*% Q_e
  # Lower right of matrix
  Q_yx_cond_theta[(len_y + 1):(len_y + len_x), (len_y + 1):(len_y + len_x)] <- Q_x + Matrix::t(Z) %*% Q_e %*% Z

  chol_Q_yx_cond_theta <- Matrix::Cholesky(Q_yx_cond_theta)

  yx_samp[i, ] <- sparseMVN::rmvn.sparse(
    n = 1,
    mu = mu_yx_cond_theta,
    CH = chol_Q_yx_cond_theta,
    prec = TRUE
  ) |>
    as.numeric()

  y_hat <- Matrix::Matrix(yx_samp[i, seq_len(len_y)])
  x <- Matrix::Matrix(yx_samp[i, len_y + seq_len(len_x)])



  while (i < n_samp) {
    i <- i + 1
    p()




    theta_samp[i, ] <- met_hast_theta(
      theta_samp[i - 1, ],
      y,
      x,
      mu_x,
      chol_Q_x,
      Z,
      chol_Q_e,
      mu_x_cond_y,
      chol_Q_x_cond_y
    )

    mu_x <- Matrix::Matrix(theta_samp[i, 1:4])
    Q_x <- Matrix::.sparseDiagonal(n = nrow(mu_x), x = exp(theta_samp[i, 5:8])^2)
    chol_Q_x <- Matrix::Cholesky(Q_x)


    Q_x_cond_y <- Q_x + Matrix::t(Z) %*% Q_e %*% Z
    chol_Q_x_cond_y <- Matrix::Cholesky(Q_x_cond_y)

    mu_x_cond_y <- Matrix::solve(Q_x_cond_y, Q_x %*% mu_x + Matrix::t(Z) %*% Q_e %*% y)


    mu_yx_cond_theta <- rbind(Z %*% mu_x_cond_y, mu_x_cond_y)

    Q_yx_cond_theta <- Matrix::Matrix(
      data = 0,
      nrow = nrow(mu_yx_cond_theta),
      ncol = nrow(mu_yx_cond_theta)
    )

    # Upper left of matrix
    Q_yx_cond_theta[1:len_y, 1:len_y] <- Q_e
    # Upper right of matrix
    Q_yx_cond_theta[1:len_y, (len_y + 1):(len_y + len_x)] <- -Q_e %*% Z
    # Lower left of matrix
    Q_yx_cond_theta[(len_y + 1):(len_y + len_x), 1:len_y] <- - Matrix::t(Z) %*% Q_e
    # Lower right of matrix
    Q_yx_cond_theta[(len_y + 1):(len_y + len_x), (len_y + 1):(len_y + len_x)] <- Q_x + Matrix::t(Z) %*% Q_e %*% Z

    chol_Q_yx_cond_theta <- Matrix::Cholesky(Q_yx_cond_theta)

    yx_samp[i, ] <- sparseMVN::rmvn.sparse(
      n = 1,
      mu = mu_yx_cond_theta,
      CH = chol_Q_yx_cond_theta,
      prec = TRUE
    ) |>
      as.numeric()

    y_hat <- Matrix::Matrix(yx_samp[i, seq_len(len_y)])
    x <- Matrix::Matrix(yx_samp[i, len_y + seq_len(len_x)])

    # if (i %% 50 == 0) message(stringr::str_c("Finished with iteration: ", i))

  }

  list(
    "theta" = theta_samp,
    "yx" = yx_samp
  )

}
