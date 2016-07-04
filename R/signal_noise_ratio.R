init_perf_fun <- function(y, X, beta, family, item_label) {
  function(param) {
    generate_ratio(y, X, beta, family, param)[[item_label]]
  }
}

# give y, X, beta, family; generate weights; outputs ratio.
generate_ratio <- function(y, X, beta, family, diag_weights, ...) {
  # if (missing(diag_weights)) diag_weights <- rnorm(nrow(X))
  n <- nrow(X)
  p <- ncol(X)
  if (length(diag_weights) != n)
    stop("'diag_weights' must have length equal to the number of rows in X.")

  W <- diag(diag_weights)
  A <- t(X) %*% W %*% X
  A_inv <- solve(A)
  # diag(A_inv)

  distFUN <- family2distFUN(family, ...)
  if (missing(beta)) beta <- rnorm(p)
  eta <- X %*% beta
  mu <- family$linkinv(eta)
  if (missing(y)) y <- purrr::map_dbl(mu, distFUN)

  z <- eta + (y - mu) / family$mu.eta(eta)  #g'(mu) = g'(g^{-1}(eta)) = mu.eta(eta)^{-1}
  beta <- A_inv %*% t(X) %*% W %*% z
  signal_noise_ratio <- abs( beta / diag(A_inv) )

  list(beta = beta, signal_noise_ratio = signal_noise_ratio,
       diag_weights = diag(W))
}
