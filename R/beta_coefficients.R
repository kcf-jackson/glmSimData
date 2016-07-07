#' Generate predictor coefficients according to the signal-noise ratio
#' @export
generate_beta <- function(X, family, f = identity, target_ratio, ...) {
  if (family$family == "gaussian") 
    return(generate_beta_gaussian(X, target_ratio, ...))

  res <- generate_weights(X, family, f, target_ratio, ...)
  my_data <- res$data
  w <- res$return$parameter
  glm_model <- glm(resp_var ~ . -1, data = my_data, family,
                    weights = ceil_exp(w))
  list(beta = glm_model$coefficients, weights = ceil_exp(w),
    data = my_data, SNR = extract_ratio(glm_model))
}
#' Optimise the weights matrix to match the target signal-noise ratio
generate_weights <- function(X, family, f = identity, target_ratio,
                          max_iter = 100, tol = 0.1, curiosity = 1000,
                          block_num = 20) {
  beta <- rnorm(ncol(X))
  my_data <- generate_response(X, beta, family, f = f)
  csignal_ratio <- init_signal_noise(my_data, family, tf = ceil_exp) %>%
                    compiler::cmpfun()
  res <- stochastic_search(n, csignal_ratio, ls_loss, target_ratio,
                           max_iter = max_iter, tol = tol,
                           curiosity = curiosity, block_num = block_num)
  if (res$loss > tol) {
    simpleWarning("Desired tolerence is not reached.")
  }
  print(res$loss)
  list(data = my_data, return = res)
}


#' Generate predictor coefficients according to the signal-noise ratio for Gaussian distribution
generate_beta_gaussian <- function(X, target_ratio, ...) {
  tX <- t(X)
  XTX_inv <- solve(tX %*% X)
  epsilon <- rnorm(nrow(X), ...)
  sigma2 <- var(epsilon)

  beta <- target_ratio * sigma2 / diag(XTX_inv) - XTX_inv %*% tX %*% epsilon
  my_data <- data.frame(resp_var = X %*% beta + epsilon, X)
  glm_model <- glm(resp_var ~ . -1, data = my_data, family = gaussian())
  list(beta = beta, weights = rep(1, nrow(X)),
    data = my_data, SNR = extract_ratio(glm_model))
}
 

#' Reduce the weights such that it rounds to multiples of 1000.
#' @export
modify_beta <- function(beta, N) {
  w <- beta$weights
  new_w <- reduce_weights(w, N)
  glm_model <- glm(resp_var ~ . -1, data = beta$data,
                    family = family, weights = new_w)
  list(beta = glm_model$coefficients, weights = new_w,
        data = beta$data, SNR = extract_ratio(glm_model))
}
#' Reduce the top N weights by 1
reduce_weights <- function(w, N, by = 1) {
  index <- tail(order(w), N)
  w[index] <- w[index] - by
  w
}
