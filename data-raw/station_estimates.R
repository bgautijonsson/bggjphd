## code to prepare `station_estimates` dataset goes here

station_estimates <-  ms_max(priors = "default")

usethis::use_data(station_estimates, overwrite = TRUE)


eta_hat <- station_estimates |>
  select(-hess) |>
  unnest(par) |>
  arrange(name, station) |>
  pull(value, name = name)

usethis::use_data(eta_hat, overwrite = TRUE)


# Q_etay <- station_estimates |>
#   select(-par) |>
#   unnest(hess) |>
#   mutate(
#     station = str_pad(station, width = 4, side = "left", pad = "0"),
#     fullname1 = str_c(name1, station),
#     fullname2 = str_c(name2, station)
#   ) |>
#   complete(fullname1, fullname2, fill = list(value = 0)) |>
#   mutate(
#     station = parse_number(fullname1),
#     name1 = str_replace(fullname1, "[0-9]+", ""),
#     name2 = str_replace(fullname2, "[0-9]+", "")
#     ) |>
#   mutate_at(
#     vars(name1, name2),
#     function(x) as_factor(x) |> fct_relevel("psi", "tau", "phi", "gamma")
#   ) |>
#   arrange(name1, name2, station) |>
#   select(-station, -name1, -name2) |>
#   pivot_wider(names_from = fullname2, values_from = value) |>
#   select(-fullname1) |>
#   as.matrix() |>
#   Matrix() |>
#   forceSymmetric()

# mats <- station_estimates |>
#   select(-par) |>
#   unnest(hess) |>
#   arrange(station) |>
#   group_by(
#     name1, name2
#   ) |>
#   group_nest() |>
#   mutate(
#     mat = map(
#       data,
#       function(data) {
#         n_rows <- nrow(data)
#
#         Diagonal(
#           n = n_rows,
#           x = data$value
#         )
#       }
#     )
#   ) |>
#   select(name1, name2, mat) |>
#   pivot_wider(names_from = name2, values_from = mat) |>
#   select(-name1)
#
# n_stations <- nrow(stations)
#
# Q_etay <- Matrix(
#   nrow = 4 * n_stations,
#   ncol = 4 * n_stations
# )
#
#
# for (i in 1:4) {
#   for (j in 1:4) {
#     x_start = (i - 1) * n_stations + 1
#     x_end = i * n_stations
#
#     y_start = (j - 1) * n_stations + 1
#     y_end = j * n_stations
#
#     Q_etay[x_start:x_end, y_start:y_end] <- mats[i, j][[1]][[1]]
#     mats[i, j][[1]][[1]] <- list(NULL)
#   }
# }
#
# Q_etay <- Matrix(Q_etay)

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
