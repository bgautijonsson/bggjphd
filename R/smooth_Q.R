#' Title
#'
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
make_Q_u <- function(x_dim, y_dim) {

  R_x <- bandSparse(
    n = x_dim,
    m = x_dim,
    k = -1:1,
    diagonals = list(
      rep(-1, x_dim),
      c(1, rep(2, x_dim - 2), 1),
      rep(-1, x_dim)
    )
  )

  R_y <- bandSparse(
    n = y_dim,
    m = y_dim,
    k = -1:1,
    diagonals = list(
      rep(-1, y_dim),
      c(1, rep(2, y_dim - 2), 1),
      rep(-1, y_dim)
    )
  )

  I_x <- Diagonal(x_dim, x = 1)
  I_y <- Diagonal(y_dim, x = 1)


  out <- kronecker(R_y, I_x) + kronecker(I_y, R_x)

  out
}



