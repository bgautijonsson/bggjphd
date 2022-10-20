#' Use STAN to fit a similar model to the latent gaussian model in Max & Smooth
#'
#' @param data The data to use in model fitting
#' @param n_chain The number of chains
#' @param parallel_chains How many chains to sample in parallel
#' @param iter_warmup Number of samples in the warmup phase
#' @param iter_sampling Number of samples after warmup
#' @param init How to initialize the parameters (See cmdstanr documentation)
#' @param refresh How often to print updates (See cmdstanr documentation)
#'
#' @return A cmdstanr model object that has been used to sample from the posterior
#' @export
#'
fit_stan <- function(data, iter_warmup = 500, iter_sampling = 500, n_chain = 4, parallel_chains = 4, init = 0, refresh = 100) {
  stan_mod <- load_model()

  data <- data |>
    arrange(station, year)

  N_obs <- nrow(data)
  N_stations <- length(unique(data$station))

  start_stop <-  data |>
    mutate(id = row_number()) |>
    group_by(station) |>
    summarise(start = min(id),
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

  stan_fit
}


#' Title
#'
#' @return Returns the Stan version of the model
#'
load_model <- function() {
  cmdstanr::cmdstan_model("inst/Stan/model.stan")
}
