## code to prepare `stations` dataset goes here

# Last group was number 11, modelled december 24th
# cur_group <- group_df |>
#   filter(group == 12)
devtools::load_all()
d <- here::here(
  "yearly_maximum_per_hour.csv"
) |>
  vroom::vroom() |>
  dplyr::filter(
    # proj_x >= cur_group$min_x,
    # proj_x <= cur_group$max_x,
    # proj_y >= cur_group$min_y,
    # proj_y <= cur_group$max_y
    # proj_x <= 100,
    # proj_y <= 100
  )


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


