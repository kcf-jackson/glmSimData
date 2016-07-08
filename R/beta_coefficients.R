#' Generate predictor coefficients according to the signal-noise ratio (SNR)
#' @param X matrix; data containing the covariates
#' @param family family object; see "?family" for more details.
#' @param target_ratio numeric vector; the desired signal-noise ratio
#' @param group boolean; Should the data be grouped or ungrouped. See details.
#' @param f function; transformation for the covariates.
#' @param ... extra parameters; see details. 
#' @details For families other than 'gaussian', the function reweights the 
#' samples such that the target signal-noise ratio is reached. This implies 
#' the data records will be treated as grouped records. If 'group' is TRUE, 
#' then the function returns the grouped data, otherwise, the function returns 
#' the ungrouped data (through duplication). \cr\cr
#' If 'family' is gaussian, one can pass the 'sd' argument through `...`. 
#' Otherwise, the function runs a stochastic search algorithm to search 
#' for the predictors coefficients, and one can pass the 'max_iter' argument 
#' (default to be 100) to `...`. \cr\cr
#' Other parameters are tol, curiosity and block_num. They are not recommended
#' to external users.
#' @export
generate_response_with_ratio <- function(
  X, family, target_ratio, group = FALSE, f = identity, ...) {
  if (family$family == "gaussian")
    return(response_with_ratio_gaussian(X, target_ratio, ...))

  res <- response_with_ratio(X, family, f, target_ratio, ...)
  my_data <- res$data
  w <- res$return$parameter
  glm_model <- glm(resp_var ~ . -1, data = my_data, family,
                    weights = ceil_exp(w))
  if (group)
    return( list(beta = glm_model$coefficients, weights = ceil_exp(w),
            data = my_data, SNR = extract_ratio(glm_model)) )

  list(beta = glm_model$coefficients, weights = rep(1, nrow(X)),
        data = ungroup_data(my_data, ceil_exp(w)),
        SNR = extract_ratio(glm_model))
}
#' [Core] Optimise the weights matrix to match the target signal-noise ratio
response_with_ratio <- function(X, family, f = identity, target_ratio,
                          max_iter = 100, tol = 0.1, curiosity = 1000,
                          block_num = 20) {
  beta <- rnorm(ncol(X))
  my_data <- generate_response(X, beta, family, f = f)
  csignal_ratio <- init_signal_noise(my_data, family, tf = ceil_exp) %>%
                    compiler::cmpfun()
  res <- stochastic_search(nrow(X), csignal_ratio, ls_loss, target_ratio,
                           max_iter = max_iter, tol = tol,
                           curiosity = curiosity, block_num = block_num)
  if (res$loss > tol) {
    simpleWarning("Desired tolerence is not reached.")
  }
  print(res$loss)
  list(data = my_data, return = res)
}
#' [Core] Generate predictor coefficients according to the signal-noise ratio for Gaussian distribution
response_with_ratio_gaussian <- function(X, target_ratio, ...) {
  tX <- t(X)
  XTX_inv <- solve(tX %*% X)
  epsilon <- rnorm(nrow(X))
  arg <- list(...)
  if ("sd" %in% names(arg))
    epsilon <- rnorm(nrow(X), sd = arg$sd)
  sigma2 <- var(epsilon)

  beta <- target_ratio * sigma2 / diag(XTX_inv) - XTX_inv %*% tX %*% epsilon
  my_data <- data.frame(resp_var = X %*% beta + epsilon, X)
  glm_model <- glm(resp_var ~ . -1, data = my_data, family = gaussian())
  list(beta = beta, weights = rep(1, nrow(my_data)),
    data = my_data, SNR = extract_ratio(glm_model))
}


#' Reduce the weights such that it rounds to 'nice' numbers.
#' @param data_model_obj An object from 'generate_response_with_ratio'.
#' @param N Number of weights to be trimmed.
#' @details Not for external users.
#' @export
reduce_data_weights <- function(data_model_obj, N) {
  w <- data_model_obj$weights
  new_w <- reduce_weights(w, N)
  glm_model <- glm(resp_var ~ . -1, data = data_model_obj$data,
                    family = family, weights = new_w)
  list(beta = glm_model$coefficients, weights = new_w,
        data = data_model_obj$data, SNR = extract_ratio(glm_model))
}
#' [Core] Reduce the top N weights by 1
reduce_weights <- function(w, N, by = 1) {
  index <- tail(order(w), N)
  w[index] <- w[index] - by
  w
}


#' Extract (ungrouped) data from the beta object
#' @param data_model_obj An object from 'generate_response_with_ratio'.
#' @details Not for external users.
#' @export
extract_ungrouped_data <- function(data_model_obj) {
  ungroup_data(data_model_obj$data, data_model_obj$weights)
}
#' Ungroup grouped data according to given weights.
ungroup_data <- function(my_data, w) {
  my_data[rep(1:nrow(my_data), w), ]
}
