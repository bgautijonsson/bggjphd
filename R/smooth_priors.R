#' Basic data-based priors for theta
#'
#' @param fit_dat Tibble containing a list column with GEV parameters for each station
#'
#' @return A vector containing priors for the latent parameters x
#' @export
#'
#' @examples
get_priors <- function(fit_dat) {

  priors <- fit_dat |>
    tidyr::unnest(par) |>
    dplyr::group_by(station) |>
    dplyr::mutate(term = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(term) |>
    dplyr::summarise(mean = mean(par),
              sd = sd(par),
              .groups = "drop")

  c(priors$mean, log(1/priors$sd))

}
