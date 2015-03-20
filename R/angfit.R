angfit <- function(X, start = list(ang=2, beta=1e5), ...) {
  nls(X[, 2] ~ beta*X[, 1]^-ang,
      start = start,
      data = X,
      ...
    )
}