#' #' Simulate data
#' #' @param no_pts integer number of data points to be simulated.
#' #' @param no_covariates number of covariates to be simulated.
#' #' @param beta_spec vector of "effect size" of the covariates, can be one of "XL", "L", "M", "S", "XS".
#' #' See details for more information of the encoding.
#' #' @param family GLM family.
#' #' @param f the function relationship between the response and the explanatory variables.
#' #' @param independent_covariates whether the covariates are independent.
#' #' @param type the type of covariates, either "numerical" or "categorical".
#' #' @param no_covariates integer; excluding constant.
#' #' @param ... extra parameters corresponding to different choices of independent_covariates.
#' #' See "?generate_independent_covariates" or "?generate_dependent_covariates" for more details.
#' # Interface
#' simulate_data <- function(no_pts = 5000, no_covariates, beta_spec,
#'   family = gaussian(), f = identity, independent_covariates = TRUE,
#'   type = "numerical", ...) {
#'
#'   if (missing(no_covariates) & missing(beta_spec))
#'     stop("One of 'no_covariates' and 'beta_spec' must be present.")
#'   if (missing(no_covariates))
#'     no_covariates <- length(beta_spec) - 1
#'   if (missing(beta_spec))
#'     beta_spec <- random_labels(no_covariates + 1)
#'
#'   if (independent_covariates) {
#'     X <- generate_independent_covariates(no_pts, no_covariates, type, ...)
#'   } else {
#'     simX <- generate_dependent_covariates(no_pts, no_covariates, type, ...)
#'     X <- simX$covariates
#'     #cov_mat <- simX$covariance_matrix
#'   }
#'
#'   invLinkFUN <- family$linkinv
#'   distFUN <- family2distFUN(family)
#'   beta_list <- generate_beta(X, beta_spec, family, invLinkFUN, distFUN, f)
#'   beta <- beta_list$beta
#'
#'   mydata0 <- generate_data(X, beta, invLinkFUN, distFUN, f = f)
#'
#'   list(data = mydata0, num_data = no_pts, num_covariates = no_covariates,
#'     beta = beta, signal_noise_ratio = beta_list$signal_noise_ratio,
#'     family = family, f = f, independent_covariates = independent_covariates,
#'     covariates_type = type)
#' }
#'

# generate_more_data <- function(sim_setup) {
#   beta <- sim_setup$beta
#   family <- sim_setup$family
#   f <- sim_setup$f
#   invLinkFUN <- family$linkinv
#   distFUN <- family2distFUN(family)
#
#   independent_covariates <- sim_setup$independent_covariates
#   if (independent_covariates) {
#     X <- generate_independent_covariates(no_pts, no_covariates)
#   } else {
#     simX <- generate_dependent_covariates(no_pts, no_covariates)
#     X <- simX$covariates
#   }
#   generate_data(X, beta, invLinkFUN, distFUN, f = f)
# }

