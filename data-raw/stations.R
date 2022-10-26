## code to prepare `stations` dataset goes here


d <- vroom::vroom(
  "/Users/bgautijonsson/Documents/Skoli/PhD/PhD_Webpage/Data/yearly_maximum_per_hour.csv"
) |>
  filter(proj_x <= 30,
         proj_y <= 30)


ids <- d |>
  distinct(station) |>
  mutate(station_new = row_number())



stations <- d |>
  inner_join(
    ids,
    by = "station"
  ) |>
  select(-station) |>
  rename(station = station_new) |>
  distinct(station, proj_x, proj_y, latitude, longitude)


usethis::use_data(stations, overwrite = TRUE)


