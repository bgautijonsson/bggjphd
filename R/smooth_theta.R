#' Title
#'
#' @return
get_theta <- function() {

  theta <- list(
    # mu_nu = c(13, 0.6, 4.6, 0),
    # log_sqrt_prec_nu = c(0, 0, 0, 0),
    log_sqrt_prec_e = rnorm(n = 4, mean = 0, sd = 0.5)
  )


  theta


}
