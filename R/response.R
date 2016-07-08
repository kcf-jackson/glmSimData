#' Simulate the response data given covariates, beta and DGP specification.
#' @export
generate_response <- function(X, beta, family, f = identity) {
  invLinkFUN = family$linkinv
  distFUN = family2distFUN(family)
  if (all.equal(f, identity)) {
    Z <- X
  } else {
    Z <- model.matrix(f, data = X)
  }
  if (ncol(Z) != length(beta))
    stop("The number of beta doesn't match the number of covariates.")

  linear_predictors <- matrix_times_vector(Z, beta)
  mu <- sapply(linear_predictors, FUN = invLinkFUN)
  y <- sapply(mu, FUN = distFUN)

  data.frame(resp_var = y, X)
}


#' Return the sampling distribution of the family object
family2distFUN <- function(family, ...) {
  distFUN <- switch(
    family$family,
      "gaussian" = function(x, ...) rnorm(1, mean = x, ...),
      #"Gamma" = function(x) {rgamma(1, shape = x, rate = 1)},
      "binomial" = function(x) rbinom(1, 1, prob = x),
      "quasibinomial" = function(x, ...) {
        param <- list(...)
        shape1 <- ifelse('shape1' %in% names(param), param$shape1, 1)
        VGAM::dbetabinom.ab(1, 1, shape1 = shape1, shape2 = shape1 * (1 / x - 1))},
      "poisson" = function(x) rpois(1, lambda = x),
      "quasipoisson" = function(x) rnbinom(1, size = 2, mu = x)
  )
  distFUN
}
