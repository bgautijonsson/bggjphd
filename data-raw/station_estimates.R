## code to prepare `station_estimates` dataset goes here
devtools::load_all()
library(progressr)
library(future)
handlers("cli")

plan(multisession, workers = 4)
with_progress({
  station_estimates <-  ms_max(priors = "default")
})
plan(sequential)


usethis::use_data(station_estimates, overwrite = TRUE)


eta_hat <- station_estimates |>
  select(-hess) |>
  unnest(par) |>
  arrange(name, station) |>
  pull(value, name = name)

usethis::use_data(eta_hat, overwrite = TRUE)


vals <- station_estimates |>
  select(-par) |>
  unnest(hess) |>
  arrange(station) |>
  select(-station) |>
  nest(data = value) |>
  mutate(
    data = map(data, unlist)
  ) |>
  pivot_wider(names_from = name2, values_from = data) |>
  select(-name1)

n_stations <- nrow(stations)

Q_etay <- bandSparse(
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
