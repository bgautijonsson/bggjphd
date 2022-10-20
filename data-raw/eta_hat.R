## code to prepare `eta_hat` dataset goes here

eta_hat <- unlist(station_estimates$par)

usethis::use_data(eta_hat, overwrite = TRUE)
