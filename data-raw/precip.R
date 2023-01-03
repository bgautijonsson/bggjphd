## code to prepare `precip` dataset goes here


d <-here::here(
  "yearly_maximum_per_hour.csv"
) |>
  vroom::vroom() |>
  dplyr::semi_join(
    stations,
    by = c("proj_x", "proj_y")
  )

ids <- d |>
  dplyr::distinct(station) |>
  dplyr::mutate(
    station_new = dplyr::row_number()
  )


precip <- d |>
  dplyr::inner_join(
    ids,
    by = "station"
  ) |>
  dplyr::select(-station) |>
  dplyr::rename(station = station_new) |>
  dplyr::select(year, station, precip)


usethis::use_data(precip, overwrite = TRUE)
