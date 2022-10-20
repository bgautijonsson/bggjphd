#' Calculate the negative log likelihood of a generalized extreme value distribution with trend using vague priors on the parameters
#'
#' @param y The block maxima we want to fit our GEV with trend to
#' @param t The time at which each y is measured. To be used with the trend parameter
#' @param par The parameters for the GEV with trent
#'
#' @return The negative log likelihood of the outcome, y, at timepoints, t, given the parameters, par.
#' @export
#'
#' @examples
neg_log_lik_gev_trend <- function(y, t, par, t0 = 1981) {

  t <- t - t0

  mu0 <- exp(par[1])
  sigma <- exp(par[2]) * mu0
  xi <- 1 / (1 + exp(-par[3])) - 0.5
  delta <- 0.016 * 1 / (1 + exp(-par[4])) - 0.008

  mu <- mu0 * (1 + delta * t)

  m <- length(y)

  z <- (y - mu) / sigma

  if (any(1 + xi * z <= 0)) return(NA)


  out <- - m * log(sigma)
  out <- out - (1 + 1/xi) * sum(log(1 + xi * z))
  out <- out - sum((1 + xi * z)^{-1/xi})

  priors <- dnorm(par[1], mean = 2.4, sd = 0.2, log = T) +
    dnorm(par[2], mean = -0.5, sd = 0.2, log = T) +
    dnorm(par[3], mean = 0, sd = 0.2, log = T) +
    dnorm(par[4], mean = 0, sd = 0.2, log = T)

  out <- out + priors

  -out
}

#' A function to calculate and return parameters and Hessian for the generalized extreme distribution with trend
#'
#' @param data The tibble on which to calculate GEV parameters
#' @param y The name of the outcome variable in the data
#'
#' @return A tibble with two list columns, one for the parameters and one for the Hessian
#'
fit_gev_trend <- function(data, ...) {


  # loc_par <- data$year
  # loc_par <- (loc_par - mean(loc_par)) / sd(loc_par)

  # m <- evd::fgev(data$precip, nsloc = loc_par)

  m <- optim(
    par = c(0, 0, 0.1, 0),
    fn = neg_log_lik_gev_trend,
    y = data$precip,
    t = data$year, hessian = T
  )

  par <- m$par
  hess <- m$hessian

  # par <-tibble(
  #   parameter = c("psi", "gamma", "tau", "phi"),
  #   estimate = par
  # ) |>
  #   slice(1, 3, 4, 2)

  tibble(hess = list(hess),
         par = list(par))

}


#' Max step of Max-and-Smooth
#'
#' @param data The data on which to perform the Max step
#'
#' @return A tibble with one row per station and nested columns containing results from the Max step
#' @export
#'
ms_max <- function() {
  precip |>
    group_by(station) |>
    group_modify(fit_gev_trend)
}
