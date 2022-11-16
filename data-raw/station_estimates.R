## code to prepare `station_estimates` dataset goes here

station_estimates <-  ms_max(priors = "default")

usethis::use_data(station_estimates, overwrite = TRUE)


eta_hat <- station_estimates |>
  select(-hess) |>
  unnest(par) |>
  arrange(name, station) |>
  pull(value, name = name)

usethis::use_data(eta_hat, overwrite = TRUE)


Q_etay <- station_estimates |>
  select(-par) |>
  unnest(hess) |>
  mutate(
    station = str_pad(station, width = 4, side = "l", pad = "0"),
    fullname1 = str_c(name1, station),
    fullname2 = str_c(name2, station)
  ) |>
  complete(fullname1, fullname2, fill = list(value = 0)) |>
  mutate(
    station = parse_number(fullname1),
    name1 = str_replace(fullname1, "[0-9]+", ""),
    name2 = str_replace(fullname2, "[0-9]+", "")
    ) |>
  mutate_at(
    vars(name1, name2),
    function(x) as_factor(x) |> fct_relevel("psi", "tau", "phi", "gamma")
  ) |>
  arrange(name1, name2, station) |>
  select(-station, -name1, -name2) |>
  pivot_wider(names_from = fullname2, values_from = value) |>
  select(-fullname1) |>
  as.matrix() |>
  Matrix() |>
  forceSymmetric()

chol_Q_etay <- Matrix::Cholesky(Q_etay)

usethis::use_data(Q_etay, overwrite = TRUE)
usethis::use_data(chol_Q_etay, overwrite = TRUE)
