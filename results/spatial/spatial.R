library(bggjphd)
# library(tidyverse)
library(progressr)
library(future)
# library(arrow)
library(here)
handlers("cli")

here("results", "spatial") |> setwd()

n_iter <- 1
plan(multisession, workers = 4)
with_progress({
  results <- ms_smooth(n_samp = n_iter, type = "spatial")
})
plan(sequential)

write_rds(results, "results/spatial/results.rds")

results |>
  subset_draws("theta") |>
  as_tibble() |>
  write_parquet("results/spatial/data/theta_results.parquet")

results |>
  filter(.iteration > n_iter/2) |>
  subset_draws(
    c("psi", "tau", "phi", "gamma")
  ) |>
  summarise_draws() |>
  select(variable, mcmc_mean = mean, mcmc_median = median) |>
  mutate(station = parse_number(variable),
         variable = str_replace(variable, "\\[.*$", "")) |>
  inner_join(
    stations,
    by = "station"
  ) |>
  inner_join(
    station_estimates |>
      select(station, par) |>
      unnest(par) |>
      rename(ml_estimate = value, variable = name),
    by = c("station", "variable")
  ) |>
  select(station, proj_x, proj_y, latitude, longitude, variable, ml_estimate, mcmc_mean) |>
  mutate(
    variable = fct_relevel(
      factor(variable),
      "psi", "tau", "phi", "gamma"
    )
  ) |>
  write_parquet("results/spatial/data/station_results.parquet")
