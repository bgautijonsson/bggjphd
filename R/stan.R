#' Title
#'
#' @param data
#' @param n_samp
#' @param n_chain
#' @param parallel_chains
#'
#' @return
#' @export
#'
#' @examples
fit_stan <- function(data, iter_warmup = 500, iter_sampling = 500, n_chain = 4, parallel_chains = 4, init = 0, refresh = 100) {
  stan_mod <- load_model()

  data <- data |>
    dplyr::arrange(station, year)

  N_obs <- nrow(data)
  N_stations <- length(unique(data$station))

  start_stop <-  data |>
    dplyr::mutate(id = row_number()) |>
    dplyr::group_by(station) |>
    dplyr::summarise(start = min(id),
                     stop = max(id))

  start <- start_stop$start
  stop <- start_stop$stop

  y <- data$precip
  year <- data$year
  station <- as.integer(as.factor(data$station))



  stan_data <- list(
    N_obs = N_obs,
    N_stations = N_stations,
    start = start,
    stop = stop,
    t0 = 1981,
    y = y,
    year = year,
    station = station
  )

  stan_fit <- stan_mod$sample(
    data = stan_data,
    refresh = refresh,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    chains = n_chain,
    parallel_chains = parallel_chains,
    init = init
  )
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
load_model <- function() {
  cmdstanr::cmdstan_model("inst/Stan/model.stan")
}
