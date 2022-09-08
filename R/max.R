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


#' The cumulative distribution function for the generalized extreme value distribution with trend parameter
#'
#' @param x The value for which to calculate P(X < x)
#' @param t The timepoint at which the value x was measured
#' @param mu0 The location parameter at timepoint t0
#' @param sigma The scale parameter
#' @param xi The shape parameter
#' @param delta The trend parameter
#' @param t0 Time time at which mu0 should be applied.
#'
#' @return Returns P(X < x) for the GEV with trend
#' @export
#'
#' @examples
p_gev_trend <- function(x, t, mu0, sigma, xi, delta, t0 = 1981) {
    t <- t - t0
    mu <- mu0 * (1 + delta * t)
    exp(-(1 + xi * (x - mu) / sigma)^(-1/xi))
}


#' A function to calculate and return parameters and Hessian for the generalized extreme distribution with trend
#'
#' @param data The tibble on which to calculate GEV parameters
#' @param y The name of the outcome variable in the data
#'
#' @return A tibble with two list columns, one for the parameters and one for the Hessian
#' @export
#'
#' @examples
fit_gev_trend <- function(data, ...) {

    # opt <- optim(neg_log_lik_gev_trend,
    #              par = c(0, 0, 0.1, 0),
    #              y = data$precip,
    #              t = data$year,
    #              hessian = T)
    #
    # par <- opt$par
    # mu <- exp(par[1])
    # sigma <- exp(par[2]) * mu
    # xi <- 1 / (1 + exp(-par[3])) - 0.5
    # delta <- 0.016 * 1 / (1 + exp(-par[4])) - 0.008

    loc_par <- data$year
    loc_par <- (loc_par - mean(loc_par)) / sd(loc_par)

    m <- evd::fgev(data$precip, nsloc = loc_par)

    par <- m$param
    hess <- solve(m$var.cov)

    tibble(hess = list(hess),
           par = list(par))

}


#' Max step of Max-and-Smooth
#'
#' @param data The data on which to perform the Max step
#'
#' @return
#' @export
#'
#' @examples
ms_max <- function(data) {
  data |>
    group_by(station) |>
    group_modify(fit_gev_trend)
}
