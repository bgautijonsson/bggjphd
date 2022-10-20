#' A function to calculate and return parameters and Hessian for the generalized extreme distribution with trend
#'
#' @param data The tibble on which to calculate GEV parameters
#' @param y The name of the outcome variable in the data
#'
#' @return A tibble with two list columns, one for the parameters and one for the Hessian
#'
fit_gev_trend <- function(data, ...) {


    loc_par <- data$year
    loc_par <- (loc_par - mean(loc_par)) / sd(loc_par)

    m <- evd::fgev(data$precip, nsloc = loc_par)

    par <- m$param
    hess <- solve(m$var.cov)

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
