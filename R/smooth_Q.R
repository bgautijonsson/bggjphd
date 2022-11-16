#' Title
#'
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
make_Q_u <- function(dim) {

  R <- bandSparse(
    n = dim,
    m = dim,
    k = -1:1,
    diagonals = list(
      rep(-1, dim),
      c(1, rep(2, dim - 2), 1),
      rep(-1, dim)
    )
  )

  I <- Diagonal(dim, x = 1)


  kronecker(R, I) + kronecker(I, R)
}



