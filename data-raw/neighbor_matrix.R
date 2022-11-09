## code to prepare `neighbor_matrix` dataset goes here

library(tidyverse)
library(here)
library(foreach)
n.cores <- parallel::detectCores() - 1


#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "FORK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)



d <- stations |>
  distinct(station, proj_x, proj_y)

ids <- d |>
  distinct(station) |>
  mutate(station_new = row_number())



d <- d |>
  inner_join(
    ids,
    by = "station"
  ) |>
  select(-station) |>
  rename(station = station_new) |>
  arrange(station)



start <- Sys.time()

neighbors <- foreach(
  i = seq_len(nrow(d))
) %dopar% {

  cur_station <- d[i, ]
  cur_x <- cur_station$proj_x
  cur_y <- cur_station$proj_y
  cur_id <- cur_station$station

  subset(d,
         subset = (abs(proj_x - cur_x) <= 1) &
           (abs(proj_y - cur_y) <= 1) &
           (abs(proj_x - cur_x) + abs(proj_y - cur_y) <= 1) &
           (station != cur_id),
         select = station,
         drop = TRUE)
}

stop <- Sys.time()

parallel::stopCluster(cl = my.cluster)

cat(paste0("Algorithm took ", as.numeric(stop - start), " seconds to run."))

neighbors <- d |>
  mutate(neighbors = neighbors) |>
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




Q_u <- bandSparse(
  n = nrow(neighbor_matrix),
  k = c(0, 1, 30),
  diagonals =
    list(
      rowSums(neighbor_matrix),
      rep(-1, nrow(neighbor_matrix)),
      rep(-1, nrow(neighbor_matrix))
    ),
  symmetric = TRUE
)



usethis::use_data(Q_u, overwrite = TRUE)

