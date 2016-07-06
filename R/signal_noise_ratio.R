#' Initialise objective function for optimisation
#' @export
init_perf_fun <- function(y, X, beta, family, item_label) {
  function(param) {
    generate_ratio(y, X, beta, family, param)[[item_label]]
  }
}


init_perf_fun_2 <- function(y, X, family) {
  my_data <- data.frame(resp = y, X)
  function(param) {
    tmp <- summary(glm(resp ~ . -1, data = my_data, family, weights = round(exp(param)) ))
    abs(tmp$coefficients[,1] / tmp$coefficients[,2])
  }
}


#' Objective function
# give y, X, beta, family; generate weights; outputs ratio.
generate_ratio <- function(y, X, beta, family, diag_weights, ...) {
  # if (missing(diag_weights)) diag_weights <- rnorm(nrow(X))
  n <- nrow(X)
  p <- ncol(X)
  if (length(diag_weights) != n)
    stop("'diag_weights' must have length equal to the number of rows in X.")

  W <- diag(exp(diag_weights))
  A <- t(X) %*% W %*% X
  A_inv <- solve(A)
  # diag(A_inv)

  distFUN <- family2distFUN(family, ...)
  if (missing(beta)) beta <- rnorm(p)
  eta <- X %*% beta
  mu <- family$linkinv(eta)
  if (missing(y)) y <- purrr::map_dbl(mu, distFUN)

  z <- eta + (y - mu) / family$mu.eta(eta)  #g'(mu) = g'(g^{-1}(eta)) = mu.eta(eta)^{-1}
  beta <- as.vector(A_inv %*% t(X) %*% W %*% z)
  signal_noise_ratio <- abs( beta / sqrt(diag(A_inv)) )

  list(beta_estimates = beta, signal_noise_ratio = signal_noise_ratio,
       diag_weights = diag(W))
}


#' Categorise signal-noise ratio
find_signal_label <- function(signal_noise_ratio) {
  # <--XS-- 0.22 --S-- 0.61 --M-- 1.65 --L-- 4.48 --XL-->
  # SN_intervals <- c(0.22, 0.61, 1.65, 4.48)  #on exponential scale
  # SN_labels <- c("XS", "S", "M", "L", "XL")
  # <--S-- 1.65 --M-- 4.48 --L-->
  SN_intervals <- c(1.65, 4.48)  #on exponential scale
  SN_labels <- c("S", "M", "L")
  for (i in seq_along(SN_intervals)) {
    if (signal_noise_ratio < SN_intervals[i])
      return(SN_labels[i])
  }
  tail(SN_labels, 1)
}
#' Categorise signal-noise ratio (vectorised)
#' @export
vec_find_signal_label <- Vectorize(find_signal_label)  #Vectorize


#' Generate some random labels
#' @export
random_labels <- function(num_label, labels = c("S", "M", "L")) {
  sample(labels, num_label, replace = TRUE)
}
