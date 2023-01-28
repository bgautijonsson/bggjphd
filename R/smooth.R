#' Perform the second step, smooth, to sample from the posterior
#'
#' @param n_samp The number of samples total (warmup + sampling).
#' @param n_chain The number of chains to sample from.
#'
#' @return Results from the Bayesian samples
#' @export
#'
ms_smooth <- function(write_to_disk = TRUE,
                      type = "spatial",
                      theta = NULL,
                      n_samp = 500,
                      n_chain = 4) {
  p <- progressr::progressor(steps = n_chain * n_samp + 1)

  out <- furrr::future_map(
    seq_len(n_chain),
    ~ {
      ms_smooth_sample(
        chain = .x,
        p = p,
        write_to_disk = write_to_disk,
        type = type,
        theta = NULL,
        n_samp = n_samp
      )
    },
    .options = furrr::furrr_options(seed = TRUE, stdout = TRUE)
  )

  p("Converting raw MCMC samples to tidy format",
    class = "sticky")

  tidy_results(out, type = type)

}

#' Helper function for performing sampling
#'
#' @param n_samp
#' @param chain
#' @param p
#'
#' @return
ms_smooth_sample <- function(chain,
                             p,
                             write_to_disk = TRUE,
                             type = "spatial",
                             theta,
                             n_samp = 500) {
  p()

  params <- init_params(type = type, n_samp = n_samp)

  if (type == "basic") {
    len_eta <- 4 * nrow(stations)
    len_nu <- 4
  } else if (type == "spatial") {
    len_eta <- 4 * nrow(stations)
    len_nu <- 0
  }

  i <- 1

  if (write_to_disk) {
    theta_samp_path <- stringr::str_c("theta_samp-",
                             chain,
                             ".csv")
    x_samp_path <- stringr::str_c("x_samp-",
                         chain,
                         ".csv")

    params$theta |>
      t() |>
      as.data.frame() |>
      data.table::fwrite(theta_samp_path, append = FALSE)

    params$x |>
      as.numeric() |>
      t() |>
      as.data.frame() |>
      data.table::fwrite(x_samp_path, append = FALSE)

  } else {
    theta_samp <- matrix(nrow = n_samp, ncol = length(params$theta))
    x_samp <- matrix(nrow = n_samp, ncol = len_eta + len_nu)

    theta_samp[i, ] <- params$theta
    x_samp[i, ] <- as.numeric(params$x)
  }


  while (i < n_samp) {
    i <- i + 1

    p()

    params <- update_theta(params, type = type)


    if (write_to_disk) {
      if (i > n_samp / 2) {
        params$theta |>
          t() |>
          as.data.frame() |>
          data.table::fwrite(theta_samp_path, append = TRUE)

        params$x |>
          as.numeric() |>
          t() |>
          as.data.frame() |>
          data.table::fwrite(x_samp_path, append = TRUE)

      }

    } else {
      theta_samp[i, ] <- params$theta
      x_samp[i, ] <- params$x

    }



  }

  if (write_to_disk) {
    theta_samp <- data.table::fread(theta_samp_path)
    x_samp <- data.table::fread(x_samp_path)
  }

  list("theta" = theta_samp,
       "x" = x_samp,
       "chain" = chain)

}
