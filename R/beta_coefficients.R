#' Generate predictor coefficients according to the signal-noise ratio
#' @export
generate_beta <- function(X, family, f = identity, target_ratio,
                          max_iter = 100, tol = 0.1, curiosity = 1000, 
                          block_num = 20, lambda = 0) {
  beta <- rnorm(ncol(X))
  y <- generate_response(X, beta, family, f = f)$resp_var
  csignal_ratio <- init_signal_noise(y, X, family, tf = ceil_exp) %>% 
                    compiler::cmpfun()
  reg_fun <- init_threshold_L1_reg(nrow(X) * 4, tf = ceil_exp)
  res <- stochastic_search(n, csignal_ratio, ls_loss, target_ratio,
                           max_iter = max_iter, tol = tol,
                           curiosity = curiosity, block_num = block_num,
                           lambda = lambda, reg_fun = reg_fun)
  if (res$loss > tol) {
    print("Desired tolerence is not reached, full object is returned.")
    return(res)
  }
  print(res$loss)
  res$parameter
}
