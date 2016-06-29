#' Simulate the response data given covariates, beta and DGP specification.
generate_data <- function(X, beta, invLinkFUN, distFUN, f) {
  if (all.equal(f, identity)) {
    Z <- X
  } else {
    Z <- model.matrix(f, data = X)
  }
  Z <- cbind(1, Z)
  if (ncol(Z) != length(beta))
    stop("The number of beta doesn't match the number of covariates.")

  linear_predictors <- matrix_times_vector(Z, beta)
  mu <- sapply(linear_predictors, FUN = invLinkFUN)
  y <- sapply(mu, FUN = distFUN)

  data.frame(resp_var = y, X)
}
