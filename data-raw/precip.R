## code to prepare `precip` dataset goes here

d <- vroom::vroom(
  "/Users/bgautijonsson/Documents/Skoli/PhD/PhD_Webpage/Data/yearly_maximum_per_hour.csv"
) |>
  semi_join(
    stations,
    by = c("proj_x", "proj_y")
  )

ids <- d |>
  distinct(station) |>
  mutate(station_new = row_number())


precip <- d |>
  inner_join(
    ids,
    by = "station"
  ) |>
  select(-station) |>
  rename(station = station_new) |>
  select(year, station, precip)


usethis::use_data(precip, overwrite = TRUE)
