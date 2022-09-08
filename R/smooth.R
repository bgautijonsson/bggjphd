ms_smooth <- function(fit_dat, n_samp = 500, n_chain = 4) {
  priors <- get_priors(fit_dat)

  eta_hat <- make_eta_hat(fit_dat)
  Q_e <- make_Q_e(fit_dat)
  chol_Q_e <- Matrix::Cholesky(Q_e)

  Z <- make_Z(eta_hat)
  nu <- make_nu()

  y <- eta_hat
  x <- nu


  out <- list()

  for (i in seq_len(n_chain)) {
    out[[i]] <- ms_smooth_sample(
      y = y,
      x = x,
      Q_e = Q_e,
      chol_Q_e = chol_Q_e,
      eta_hat = eta_hat,
      Z = Z,
      priors = priors,
      n_samp = n_samp
    )
  }

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

ms_smooth_sample <- function(y, x, Q_e, chol_Q_e, eta_hat, Z, priors, n_samp = 500) {

  theta <- rnorm(8, mean = priors, sd = 4)
  theta_samp <- matrix(nrow = n_samp, ncol = length(theta))

  yx_samp <- matrix(nrow = n_samp, ncol = nrow(y) + nrow(x))

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
  Q_yx_cond_theta[1:nrow(y), 1:nrow(y)] <- Q_e
  # Upper right of matrix
  Q_yx_cond_theta[1:nrow(y), (nrow(y) + 1):(nrow(y) + nrow(x))] <- -Q_e %*% Z
  # Lower left of matrix
  Q_yx_cond_theta[(nrow(y) + 1):(nrow(y) + nrow(x)), 1:nrow(y)] <- - Matrix::t(Z) %*% Q_e
  # Lower right of matrix
  Q_yx_cond_theta[(nrow(y) + 1):(nrow(y) + nrow(x)), (nrow(y) + 1):(nrow(y) + nrow(x))] <- Q_x + Matrix::t(Z) %*% Q_e %*% Z

  chol_Q_yx_cond_theta <- Matrix::Cholesky(Q_yx_cond_theta)

  yx_samp[i, ] <- sparseMVN::rmvn.sparse(
    n = 1,
    mu = mu_yx_cond_theta,
    CH = chol_Q_yx_cond_theta,
    prec = TRUE
  ) |>
    as.numeric()

  y <- Matrix::Matrix(yx_samp[i, seq_len(nrow(y))])
  x <- Matrix::Matrix(yx_samp[i, nrow(y) + seq_len(nrow(x))])



  while (i < n_samp) {
    i <- i + 1




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
    Q_yx_cond_theta[1:nrow(y), 1:nrow(y)] <- Q_e
    # Upper right of matrix
    Q_yx_cond_theta[1:nrow(y), (nrow(y) + 1):(nrow(y) + nrow(x))] <- -Q_e %*% Z
    # Lower left of matrix
    Q_yx_cond_theta[(nrow(y) + 1):(nrow(y) + nrow(x)), 1:nrow(y)] <- - Matrix::t(Z) %*% Q_e
    # Lower right of matrix
    Q_yx_cond_theta[(nrow(y) + 1):(nrow(y) + nrow(x)), (nrow(y) + 1):(nrow(y) + nrow(x))] <- Q_x + Matrix::t(Z) %*% Q_e %*% Z

    chol_Q_yx_cond_theta <- Matrix::Cholesky(Q_yx_cond_theta)

    yx_samp[i, ] <- sparseMVN::rmvn.sparse(
      n = 1,
      mu = mu_yx_cond_theta,
      CH = chol_Q_yx_cond_theta,
      prec = TRUE
    ) |>
      as.numeric()

    y <- Matrix::Matrix(yx_samp[i, seq_len(nrow(y))])
    x <- Matrix::Matrix(yx_samp[i, nrow(y) + seq_len(nrow(x))])

    if (i %% 50 == 0) print(str_c("Finished with iteration: ", i))

  }

  list(
    "theta" = theta_samp,
    "yx" = yx_samp
  )

}
