#' Take the raw results from the MCMC sampler and convert it to a tidy draws_df object
#'
#' @param results
#'
#' @return
#' @export
#'
#' @examples
tidy_results <- function(results) {

  results |>
    map(tidy_helper) |>
    bind_rows() |>
    arrange(chain, iter) |>
    mutate(draw = row_number()) |>
    rename(.iteration = iter, .chain = chain, .draw = draw) |>
    as_draws_df()

}


#' Function to be applied to each chain's results and reduced into one data frame
#'
#' @param datalist
#'
#' @return
#' @export
#'
#' @examples
tidy_helper <- function(datalist) {

  theta <- datalist$theta |>
    as.data.frame()

  names(theta) <- str_c(
    "theta[",
    seq_len(ncol(theta)),
    "]"
  )

  x <- datalist$x |>
    as.data.frame()

  names(x) <- c(
    str_c(
      rep(
        c(
          "psi",
          "tau",
          "phi",
          "gamma"
        ),
        times = nrow(stations)
      ),
      rep(
        str_c(
          "[",
          seq_len(nrow(stations)),
          "]"
        ),
        each = 4
      )
    ),
    c(
      "mu_psi",
      "mu_tau",
      "mu_phi",
      "mu_gamma"
    )
  )

  cbind(theta, x) |>
    as_tibble() |>
    mutate(iter = row_number(),
           chain = datalist$chain) |>
    select(iter, everything())

}


