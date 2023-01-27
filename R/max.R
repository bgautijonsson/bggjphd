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
neg_log_lik_gev_trend <- function(y, t, par, priors, links, t0 = 1981) {

  t <- t - t0

  mu0 <- exp(par[1])
  sigma <- exp(par[2] + par[1])
  xi <- link_shape_inverse(par[3])
  delta <- link_trend_inverse(par[4])

  mu <- mu0 * (1 + delta * t)

  m <- length(y)

  z <- (y - mu) / sigma

  if (any(1 + xi * z <= 0)) return(NA)

  out <- evd::dgev(x = y, loc = mu, scale = sigma, shape = xi, log = TRUE) |>
    sum()

  prior_likelihood <- priors$location(par[1]) +
    priors$scale(par[2]) +
    priors$shape(par[3]) +
    priors$trend(par[4])

  out <- out + prior_likelihood

  -out
}


#' Title
#'
#' @param xi
#' @param a
#' @param b
#' @param c
#'
#' @return
#' @export
#'
#' @examples
link_shape <- function(xi, a = 0.0624, b = 0.3956, c = 0.8, scale = 0.5) {
  b <- -log(1 - (1/2)^c) * (1 - (1/2)^c) * 2^(c - 1) / c
  a <- -b * log(-log(1 - (1/2)^c))

  out <- a + b * (log(-log(1 - (xi + 1/2)^c)))
  # out <- log((scale + xi) / (scale - xi))
  out
}

#' Title
#'
#' @param phi
#' @param a
#' @param b
#' @param c
#'
#' @return
#' @export
#'
#' @examples
link_shape_inverse <- function(phi, a = 0.0624, b = 0.3956, c = 0.8, scale = 0.5) {
  b <- -log(1 - (1/2)^c) * (1 - (1/2)^c) * 2^(c - 1) / c
  a <- -b * log(-log(1 - (1/2)^c))

  (1 - exp(-exp((phi - a)/b))) ^ (1 / c) - 1/2
  # 2 * scale * 1 / (1 + exp(-phi)) - scale
}

#' Title
#'
#' @param shape
#'
#' @return
#' @export
#'
#' @examples
prior_shape <- function(phi, alpha = 4, beta = 4, c = 0.8) {
  b <- -log(1 - (1/2)^c) * (1 - (1/2)^c) * 2^(c - 1) / c
  a <- -b * log(-log(1 - (1/2)^c))

  out <- gamma(a + b) / (gamma(a) * gamma(b) * b * c)
  out <- out * (link_shape_inverse(phi) + 1/2) ^ (alpha - c)
  out <- out * (1/2 - link_shape_inverse(phi)) ^ (beta - 1)
  out <- out * exp( (phi - a) / b - exp( (phi - a) / b ))
  log(out)
}

#' Title
#'
#' @param delta
#' @param d0
#'
#' @return
#' @export
#'
#' @examples
link_trend <- function(delta, d0 = 0.008, scale = 0.01) {
  out <- 1/2 * d0 * (log(d0 + delta) - log(d0 - delta))
  # out <- log((scale + delta) / (scale - delta))

  out
}

#' Title
#'
#' @param delta
#' @param d0
#'
#' @return
#' @export
#'
#' @examples
link_trend_inverse <- function(gamma, d0 = 0.008, scale = 0.01) {
  out <- d0 * (exp(2 * gamma / d0) - 1) / (exp(2 * gamma / d0) + 1)
  # 2 * scale * 1 / (1 + exp(-gamma)) - scale

  out
}

#' Title
#'
#' @param trend
#'
#' @return
#' @export
#'
#' @examples
prior_trend <- function(gamma, d0 = 0.008) {
  dnorm(gamma, mean = 0, sd = d0 / 2, log = TRUE)
}

#' A function to calculate and return parameters and Hessian for the generalized extreme distribution with trend
#'
#' @param data The tibble on which to calculate GEV parameters
#' @param y The name of the outcome variable in the data
#'
#' @return A tibble with two list columns, one for the parameters and one for the Hessian
#'
fit_gev_trend <- function(data, priors = "default", ...) {


  if (identical(priors, "default")) {
    priors <- list(
      "location" = function(x) 0,
      "scale" = function(x) 0,
      "shape" = function(x) 0,
      "trend" = function(x) 0
    )
  }

  gev_fit <- evd::fgev(data$precip)

  gev_estimates <- gev_fit$estimate

  loc_start <- log(gev_estimates[1])
  scale_start <- log(gev_estimates[2]) - loc_start
  shape_start <- link_shape(gev_estimates[3])

  if (!is.finite(shape_start)) shape_start <- 0.1
  trend_start <- 0

  starts <- c(
    loc_start,
    scale_start,
    shape_start,
    trend_start
  )


  m <- optim(
    par = starts,
    fn = neg_log_lik_gev_trend,
    y = data$precip,
    priors = priors,
    t = data$year,
     hessian = TRUE
  )

  param_names <- c("psi", "tau", "phi", "gamma")

  par <- tibble(
    name = param_names |>
      forcats::as_factor() |>
      forcats::fct_relevel("psi", "tau", "phi", "gamma"),
    value = m$par
  )

  hess <- m$hessian |>
    as.data.frame() |>
    as_tibble() |>
    set_names(
      param_names
    ) |>
    mutate(
      name1 = param_names
    ) |>
    pivot_longer(c(-name1), names_to = "name2", values_to = "value") |>
    mutate_at(
      vars(name1, name2),
      function(x) {
        x |>
        forcats::as_factor() |>
        forcats::fct_relevel("psi", "tau", "phi", "gamma")
      }
      )


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
ms_max <- function(data = NULL, priors = "default") {

  if (is.null(data)) {
    data <- precip
  }

  p <- progressr::progressor(steps = length(unique(data$station)))

  data |>
    group_by(station) |>
    group_nest() |>
    mutate(
      results = furrr::future_map(
        data,
        ~ {
          p()
          fit_gev_trend(.x, priors = priors)
        }
      )
    ) |>
    select(-data) |>
    unnest(results) |>
    ungroup()
}
