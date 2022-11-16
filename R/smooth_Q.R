#' Title
#'
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
make_Q_u <- function(dim) {
  n <- dim

  R <- bandSparse(
    n = n,
    m = n,
    k = -1:1,
    diagonals = list(
      rep(-1, n),
      c(1, rep(2, n - 2), 1),
      rep(-1, n)
    )
  )

  I <- Diagonal(n, x = 1)


  kronecker(R, I) + kronecker(I, R)
}



