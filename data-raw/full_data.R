## code to prepare `full_data` dataset goes here

full_data <- vroom::vroom(
  "/Users/bgautijonsson/Documents/Skoli/PhD/PhD_Webpage/Data/yearly_maximum_per_hour.csv"
) |>
  dplyr::group_by(station, proj_x, proj_y, latitude, longitude) |>
  dplyr::summarise(
    min_precip = min(precip),
    max_precip = max(precip),
    .groups = "drop"
  )


usethis::use_data(full_data, overwrite = TRUE)
