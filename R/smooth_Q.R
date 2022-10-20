make_R <- function(dim) {
  n <- 5

  R <- bandSparse(
    n = n,
    m = n,
    k = -1:1,
    diagonals = list(
      rep(-1, n),
      rep(2, n),
      rep(-1, n)
    )
  )

  I <- Diagonal(n, x = 1)


  kronecker(R, I) + kronecker(I, R)
}


