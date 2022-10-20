## code to prepare `Q_etay` dataset goes here

Q_etay <- bdiag(
  station_estimates$hess
)

chol_Q_etay <- Matrix::Cholesky(Q_etay)

usethis::use_data(Q_etay, overwrite = TRUE)
usethis::use_data(chol_Q_etay, overwrite = TRUE)
