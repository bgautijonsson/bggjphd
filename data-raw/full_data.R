## code to prepare `full_data` dataset goes here

full_data <- here::here(
  "yearly_maximum_per_hour.csv"
) |>
  vroom::vroom() |>
  dplyr::group_by(station, proj_x, proj_y, latitude, longitude) |>
  dplyr::summarise(
    min_precip = min(precip),
    max_precip = max(precip),
    .groups = "drop"
  )


usethis::use_data(full_data, overwrite = TRUE)
