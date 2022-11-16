#' Title
#'
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
propose_theta <- function(params) {
  theta <- params$theta

  if (params$iter < params$n_samp / 2) {
    if (params$accept > 0.44) {
      params$mult <- params$mult * 1.01
    } else {
      params$mult <- params$mult * 0.99
    }
  }

  d <- length(theta)
  mh_sd <-  params$mult * d ^(-1/2)

  mh_samples <- rnorm(n = d, mean = 0, sd = mh_sd)

  theta_prop <- theta + mh_samples

  params$theta_prop <- theta_prop

  params
}
