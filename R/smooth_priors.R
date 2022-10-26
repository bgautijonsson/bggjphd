#' Basic data-based priors for theta
#'
#' @param station_estimates Tibble containing a list column with GEV parameters for each station
#'
#' @return A vector containing the prior parameter vector theta
#'
get_priors <- function() {

  priors <- station_estimates |>
    unnest(par) |>
    group_by(station) |>
    mutate(term = row_number()) |>
    ungroup() |>
    group_by(term) |>
    summarise(mean = mean(par),
              var = var(par),
              .groups = "drop")

  list(
    mu_nu = priors$mean,
    log_prec_nu = -log(priors$var * 10)
  )



}

