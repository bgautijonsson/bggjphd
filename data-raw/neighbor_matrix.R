## code to prepare `neighbor_matrix` dataset goes here


neighbors <- stations |>
  distinct(station, neighbors) |>
  unnest(neighbors) |>
  mutate(value = 1)

neighbor_matrix <- crossing(
  station = unique(neighbors$station),
  neighbors = unique(neighbors$neighbors)
) |>
  left_join(
    neighbors,
    by = c("station", "neighbors")
  ) |>
  mutate(value = coalesce(value, 0)) |>
  pivot_wider(names_from = neighbors, values_from = value) |>
  select(-station)|>
  as.matrix() |>
  Matrix()

usethis::use_data(neighbor_matrix, overwrite = TRUE)
