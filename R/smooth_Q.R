#' Title
#'
#' @param dim
#'
#' @return
#' @import Matrix
make_Q_u <- function(x_dim, y_dim) {

  R_x <- Matrix::bandSparse(
    n = x_dim,
    m = x_dim,
    k = -1:1,
    diagonals = list(
      rep(-1, x_dim),
      c(1, rep(2, x_dim - 2), 1),
      rep(-1, x_dim)
    )
  )

  R_y <- Matrix::bandSparse(
    n = y_dim,
    m = y_dim,
    k = -1:1,
    diagonals = list(
      rep(-1, y_dim),
      c(1, rep(2, y_dim - 2), 1),
      rep(-1, y_dim)
    )
  )

  I_x <- Matrix::Diagonal(x_dim, x = 1)
  I_y <- Matrix::Diagonal(y_dim, x = 1)


  out <- Matrix::kronecker(R_y, I_x) + Matrix::kronecker(I_y, R_x)

  out
}



