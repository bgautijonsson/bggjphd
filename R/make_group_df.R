library(tidyverse)
crossing(
  x = 1:150,
  y = 1:200
) |>
  mutate(
    x_group = floor((x - 1) / 50) + 1,
    y_group = floor((y - 1) / 50) + 1,
    group = paste(x_group, y_group) |> as_factor() |> as.numeric()
  ) |>
  ggplot(aes(x, y, fill = group)) +
  geom_raster() +
  coord_cartesian(expand = FALSE) +
  theme(
    legend.position = "none"
  )

group_df <- crossing(
  x = 1:150,
  y = 1:200
) |>
  mutate(
    x_group = floor((x - 1) / 50) + 1,
    y_group = floor((y - 1) / 50) + 1,
    group = paste(x_group, y_group) |> as_factor() |> as.numeric()
  ) |>
  select(
    proj_x = x,
    proj_y = y,
    group
  ) |>
  group_by(group) |>
  reframe(
    min_x = min(proj_x),
    max_x = max(proj_x),
    min_y = min(proj_y),
    max_y = max(proj_y),
    n_stations = n()
  )
