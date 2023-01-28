## code to prepare `neighbor_matrix` dataset goes here

n.cores <- parallel::detectCores() - 1


#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "FORK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)



d <- stations |>
  dplyr::distinct(station, proj_x, proj_y)

ids <- d |>
  dplyr::distinct(station) |>
  dplyr::mutate(station_new = dplyr::row_number())



d <- d |>
  dplyr::inner_join(
    ids,
    by = "station"
  ) |>
  dplyr::select(-station) |>
  dplyr::rename(station = station_new) |>
  dplyr::arrange(station)



library(foreach)
start <- Sys.time()
neighbors <- foreach(
  i = seq_len(nrow(d))
) %dopar% {

  cur_station <- d[i, ]
  cur_x <- cur_station$proj_x
  cur_y <- cur_station$proj_y
  cur_id <- cur_station$station

  subset(d,
         subset = (abs(proj_x - cur_x) <= 2) &
           (abs(proj_y - cur_y) <= 2) &
           (abs(proj_x - cur_x) + abs(proj_y - cur_y) <= 2) &
           (station != cur_id),
         select = station,
         drop = TRUE)
}

stop <- Sys.time()

parallel::stopCluster(cl = my.cluster)

cat(paste0("Algorithm took ", as.numeric(stop - start), " seconds to run."))

neighbor_types <- dplyr::tribble(
  ~diff_x, ~diff_y, ~type,
  -2, 0, "ww",
  -1, -1, "sw",
  -1, 0, "w",
  -1, 1, "nw",
  0, -2, "ss",
  0, -1, "s",
  0, 1, "n",
  0, 2, "nn",
  1, -1, "se",
  1, 0, "e",
  1, 1, "ne",
  2, 0, "ee"
)

usethis::use_data(neighbor_types, overwrite = TRUE)

twelve_neighbors <- d |>
  dplyr::mutate(neighbor = neighbors) |>
  dplyr::distinct(station, neighbor) |>
  tidyr::unnest(neighbor) |>
  dplyr::inner_join(
    stations |>
      dplyr::select(
        station, station_x = proj_x, station_y = proj_y
      ),
    by = "station"
  ) |>
  dplyr::inner_join(
    stations |>
      dplyr::select(
        neighbor = station,
        neighbor_x = proj_x,
        neighbor_y = proj_y
      ),
    by = "neighbor"
  ) |>
  dplyr::mutate(
    diff_x = neighbor_x - station_x,
    diff_y = neighbor_y - station_y
    ) |>
  dplyr::select(station, neighbor, diff_x, diff_y) |>
  dplyr::inner_join(
    neighbor_types,
    by = c("diff_x", "diff_y")
  ) |>
  dplyr::select(station, neighbor, type) |>
  dplyr::mutate(type = forcats::as_factor(type))

usethis::use_data(twelve_neighbors, overwrite = TRUE)

