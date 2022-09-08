#' Title
#'
#' @param fit_dat
#' @param priors
#'
#' @return
#' @export
#'
#' @examples
make_matrices <- function(fit_dat, priors) {

  eta_hat <- make_eta_hat(fit_dat)
  Q_e <- make_Q_e(fit_dat)
  chol_Q_e <- make_chol_Q_e(fit_dat)

  Z <- make_Z(eta_hat)

  nu <- make_nu()



  out <- list(
    "eta_hat" = eta_hat,
    "chol_Q_e" = chol_Q_e,
    "Z" = Z
  )

  out
}

#' Title
#'
#' @param fit_dat
#'
#' @return
#' @export
#'
#' @examples
make_eta_hat <- function(fit_dat) {

  Matrix::Matrix(
    unlist(fit_dat$par),
    ncol = 1
  )

}

#' Title
#'
#' @param fit_dat
#'
#' @return
#' @export
#'
#' @examples
make_Q_e <- function(fit_dat) {
  Matrix::bdiag(
    fit_dat$hess
  )
}

make_chol_Q_e <- function(fit_dat) {

  Matrix::bdiag(
    fit_dat$hess |> purrr::map(Matrix::chol)
  )

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
make_nu <- function() {
  Matrix::Matrix(
    data = 0,
    nrow = 4,
    ncol = 1
  )
}

#' Title
#'
#' @param eta_hat
#'
#' @return
#' @export
#'
#' @examples
make_Z <- function(eta_hat) {
  rows <- nrow(eta_hat)
  n_station <- rows/4

  Z <- Matrix::Matrix(
    data = 0,
    nrow = rows,
    ncol = 4
  )

  for (i in 1:4) Z[i + (seq_len(n_station) - 1) * 4, i] <- 1

  Z
}


