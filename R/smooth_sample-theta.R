#' Title
#'
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
propose_theta <- function(theta) {
  d <- length(theta)
  mh_sd <-  d ^(-1/2)

  mh_samples <- rnorm(n = d, mean = 0, sd = mh_sd)

  theta_prop <- theta + mh_samples

  theta_prop
}
