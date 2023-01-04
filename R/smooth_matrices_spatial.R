#' Title
#'
#' @return
init_theta_spatial <- function() {

  rnorm(n = 4, mean = 0, sd = 0.5)


}


#' Title
#'
#' @param Z
#' @param nu
#'
#' @return
#' @export
#'
#' @examples
init_eta_spatial <- function(theta) {
  n_stations <- nrow(stations)
  n_param <- 4 * n_stations

  log_sd <- -theta/2
  sd <- exp(log_sd)
  sd <- rep(sd, each = n_stations)

  out <- rnorm(
    n = n_param,
    mean = 0,
    sd = sd
  )

  Matrix(
    data = out,
    ncol = 1
  )
}

#' Title
#'
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
make_Q_e_spatial <- function(theta) {
  n_stations <- nrow(stations)

  x_dim <- stations |>
    pull(proj_x) |>
    unique() |>
    length()

  y_dim <- stations |>
    pull(proj_y) |>
    unique() |>
    length()

  n_params <- 4 * n_stations

  log_prec <- theta
  prec <- exp(log_prec)

  Q_u <- make_Q_u(x_dim, y_dim)


  Q_e <- bdiag(
    list(
      prec[1] * Q_u,
      prec[2] * Q_u,
      prec[3] * Q_u,
      prec[4] * Q_u
    )
  )

  Q_e
}

#' Title
#'
#' @param mu_x
#' @param B
#' @param Q_x
#'
#' @return
#' @export
#'
#' @examples
make_Q_eta_cond_etahat_spatial <- function(Q_e) {
  Q_e + Q_etay
}

#' Title
#'
#' @param mu_nu
#' @param Q_nu
#' @param B
#' @param Q_x_cond_etahat
#'
#' @return
#' @export
#'
#' @examples
make_mu_eta_cond_etahat_spatial <- function(Q_eta_cond_etahat) {
  Matrix::solve(Q_eta_cond_etahat, Q_etay %*% eta_hat)
}






