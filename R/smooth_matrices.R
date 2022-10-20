#' Make the model matrix for eta
#'
#' @param fmla
#'
#' @return
#' @export
#'
#' @examples
make_Z <- function(fmla = NULL) {

  if (is.null(fmla)) {
    fmla <- station ~ param - 1
  }



  out <- sparse.model.matrix(
    fmla,
    data = stations |>
      crossing(
        param = c("psi", "tau", "phi", "gamma")
      )
  )


  out
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
make_eta <- function(Z, nu, theta) {
  n_stations <- nrow(stations)
  n_param <- nrow(Z)

  mean <- Z %*% nu

  log_sqrt_prec <- theta$log_sqrt_prec_e
  sd <-exp(-log_sqrt_prec)
  sd <- rep(sd, times = n_stations)

  out <- rnorm(
    n = n_param,
    mean = as.numeric(mean),
    sd = sd
  )

  Matrix(
    data = out,
    ncol = 1
  )
}


#' Create the vector nu
#'
#' Creates a vector containing the fixed and mixed effects for modeling
#' the data-level parameters
#'
#' @return
#' @export
#'
#' @examples
make_nu <- function(priors) {

  n <- length(priors$mu_nu)

  nu <- rnorm(n = n, mean = priors$mu_nu, sd = exp(-priors$log_sqrt_prec_nu))

  Matrix(
    data = nu,
    ncol = 1
  )
}




#' Make the matrix B
#'
#' @param eta_hat
#' @param nu
#'
#' @return
#' @export
#'
#' @examples
make_B <- function(Z) {
  p <- nrow(Z)
  q <- ncol(Z)

  I <- Diagonal(n = p, x = 1)
  O <- Matrix(data = 0, nrow = p, ncol = q)

  B <- cbind(I, O)

  B
}


#' Title
#'
#' @param nu
#' @param eta
#'
#' @return
#' @export
#'
#' @examples
make_x <- function(nu, eta) {
  rbind(eta, nu)
}


#' Title
#'
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
make_mu_nu <- function(priors) {
  Matrix(
    priors$mu_nu,
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
make_Q_nu <- function(priors) {
  .sparseDiagonal(
    n = 4,
    x = exp(2 * priors$log_sqrt_prec_nu)
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
make_Q_e <- function(theta) {
  n_stations <- nrow(stations)

  log_sqrt_prec <- theta
  prec <- exp(2 * log_sqrt_prec)
  prec <- rep(prec, times = n_stations)

  .sparseDiagonal(
    x = prec
  )
}

#' Title
#'
#' @param Q_e
#' @param Q_nu
#' @param Z
#'
#' @return
make_Q_x <- function(Q_e, Z, Q_nu) {
  t <- Matrix::t

  len_eta <- ncol(Q_e)
  len_nu <- ncol(Q_nu)

  Q_x <- Matrix(
    data = 0,
    nrow = len_eta + len_nu,
    ncol = len_eta + len_nu
  )

  # Upper left of matrix
  Q_x[1:len_eta, 1:len_eta] <- Q_e
  # Upper right of matrix
  Q_x[1:len_eta, (len_eta + 1):(len_eta + len_nu)] <- -Q_e %*% Z
  # Lower left of matrix
  Q_x[(len_eta + 1):(len_eta + len_nu), 1:len_eta] <- - t(Z) %*% Q_e
  # Lower right of matrix
  Q_x[(len_eta + 1):(len_eta + len_nu), (len_eta + 1):(len_eta + len_nu)] <- Q_nu + t(Z) %*% Q_e %*% Z

  Q_x
}

#' Title
#'
#' @param Z
#' @param mu_nu
#'
#' @return
#' @export
#'
#' @examples
make_mu_x <- function(Z, nu) {
  rbind(
    Z %*% nu,
    nu
  )
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
make_Q_x_cond_etahat <- function(mu_x, Q_x, B) {
  t <- Matrix::t

  Q_x_cond_etahat <- Q_x + t(B) %*% Q_etay %*% B
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
make_mu_x_cond_etahat <- function(mu_x, Q_x, Q_x_cond_etahat, B) {
  t <- Matrix::t
  Matrix::solve(
    Q_x_cond_etahat,
    Q_x %*% mu_x + t(B) %*% Q_etay %*% eta_hat
  )
}


#' Title
#'
#' @param Q_x_cond_y
#' @param Q_x
#' @param mu_x
#' @param Z
#' @param Q_e
#' @param y
#'
#' @return
#' @export
#'
#' @examples
make_mu_nu_cond_etahat <- function(Q_x_cond_y, Q_x, mu_x, Z, Q_e, y) {
  t <- Matrix::t
  solve(Q_x_cond_y, Q_x %*% mu_x + t(Z) %*% Q_e %*% y)
}




