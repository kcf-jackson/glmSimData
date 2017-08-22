#' Generate independent features - numerical or (ordinal) categorical
#' @param n integer; number of data points to be simulated.
#' @param p integer; number of covariates.
#' @param distn One of 'gaussian', 'student-t', 'pareto', 'uniform', 'discrete_uniform', 'poisson', 'gamma'.
#' @param num_categories integer; number of categories for 'discrete_uniform'.
#' @param ... Extra parameters for distributions other than 'gaussian' and 'uniform'.
#' @details See "?rnorm", "?rt", "?VGAM::rpareto", "?runif", "?rpois", "?rgamma"
#' for more details on the extra parameters. If 'discrete_uniform' is chosen, one
#' can specify the distribution for the categories through the parameter 'prob'.
#' @export
generate_features <- function(n, p, distn = "gaussian", num_categories, ...) {
  distn_list <- c("gaussian", "student-t", "pareto", "uniform",
                  "discrete_uniform", "poisson", "gamma")
  if ((distn == 'discrete_uniform') && (missing(num_categories))) {
    num_categories <- 5
    print("parameter 'num_categories' is not provided, it is set to 5 by default.")
  }
  if (!(distn %in% distn_list))
    stop(
      paste(c("Must be one of '", paste(distn_list, collapse = "', '"), "'.")),
      call. = FALSE)

  num_sim <- n * p
  if (distn == "discrete_uniform") {
    # Categorical distribution
    samples <- sample(x = num_categories, size = num_sim, replace = TRUE, ...) - 1
    res <- data.frame(matrix(samples, nrow = n, ncol = p))
    # res <- purrr::map(res, as.factor) %>% data.frame()  # turn into factors
  } else {
    # Numerical distribution
    rdist <- switch(distn,
      "gaussian" = rnorm, "student-t" = rt, "pareto" = VGAM::rpareto,
      "uniform" = runif, "poisson" = rpois, "gamma" = rgamma
    )
    samples <- rdist(num_sim, ...)
    res <- data.frame(matrix(samples, nrow = n, ncol = p))
  }
  res
}
