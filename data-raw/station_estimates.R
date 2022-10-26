## code to prepare `station_estimates` dataset goes here
priors <- list(
  "location" = function(x) 0,
  "scale" = function(x) 0,
  "shape" = function(x) 0,
  "trend" = function(x) 0
)

station_estimates <-  ms_max(priors = priors)

usethis::use_data(station_estimates, overwrite = TRUE)


eta_hat <- unlist(station_estimates$par)

usethis::use_data(eta_hat, overwrite = TRUE)

Q_etay <- bdiag(
  station_estimates$hess
)

chol_Q_etay <- Matrix::Cholesky(Q_etay)

usethis::use_data(Q_etay, overwrite = TRUE)
usethis::use_data(chol_Q_etay, overwrite = TRUE)
