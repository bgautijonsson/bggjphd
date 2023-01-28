## code to prepare `station_estimates` dataset goes here

library(bggjphd)
library(progressr)
library(future)
handlers("cli")

priors <- list(
  "location" = function(x) 0,
  "scale" = function(x) 0,
  "shape" = prior_shape,
  "trend" = prior_trend
)

plan(multisession, workers = 4)
with_progress({
  station_estimates <-  ms_max(priors = priors)
})
plan(sequential)


usethis::use_data(station_estimates, overwrite = TRUE)


eta_hat <- station_estimates |>
  dplyr::select(-hess) |>
  tidyr::unnest(par) |>
  dplyr::arrange(name, station) |>
  dplyr::pull(value, name = name)

usethis::use_data(eta_hat, overwrite = TRUE)


vals <- station_estimates |>
  dplyr::select(-par) |>
  tidyr::unnest(hess) |>
  dplyr::arrange(station) |>
  dplyr::select(-station) |>
  tidyr::nest(data = value) |>
  dplyr::mutate(
    data = purrr::map(data, unlist)
  ) |>
  tidyr::pivot_wider(names_from = name2, values_from = data) |>
  dplyr::select(-name1)

n_stations <- nrow(stations)

Q_etay <- Matrix::bandSparse(
  n = 4 * n_stations,
  m = 4 * n_stations,
  k = c(
    0,
    c(-1, 1) * n_stations,
    c(-2, 2) * n_stations,
    c(-3, 3) * n_stations
  ),
  diagonals = list(
    c(vals[1, 1][[1]][[1]], vals[2, 2][[1]][[1]], vals[3, 3][[1]][[1]], vals[4, 4][[1]][[1]]),
    c(vals[2, 1][[1]][[1]], vals[3, 2][[1]][[1]], vals[4, 3][[1]][[1]]),
    c(vals[1, 2][[1]][[1]], vals[2, 3][[1]][[1]], vals[3, 4][[1]][[1]]),
    c(vals[3, 1][[1]][[1]], vals[4, 2][[1]][[1]]),
    c(vals[1, 3][[1]][[1]], vals[2, 4][[1]][[1]]),
    vals[4, 1][[1]][[1]],
    vals[1, 4][[1]][[1]]
  )
)


chol_Q_etay <- Matrix::Cholesky(Q_etay)

usethis::use_data(Q_etay, overwrite = TRUE)
usethis::use_data(chol_Q_etay, overwrite = TRUE)
