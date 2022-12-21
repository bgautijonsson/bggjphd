## code to prepare `stations` dataset goes here


d <- here::here(
  "yearly_maximum_per_hour.csv"
) |>
  vroom::vroom() |>
  dplyr::filter(proj_x <= 50,
                proj_y <= 50)


ids <- d |>
  dplyr::distinct(station) |>
  dplyr::mutate(
    station_new = dplyr::row_number()
  )



stations <- d |>
  dplyr::inner_join(
    ids,
    by = "station"
  ) |>
  dplyr::select(-station) |>
  dplyr::rename(station = station_new) |>
  dplyr::distinct(station, proj_x, proj_y, latitude, longitude)


usethis::use_data(stations, overwrite = TRUE)


