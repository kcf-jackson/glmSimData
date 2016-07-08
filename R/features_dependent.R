#' Generate dependent covariates - numerical or (ordinal) categorical
#' @param no_pts numeric; number of points to simulate.
#' @param no_covariates numeric; number of covariates required.
#' @param type character; 'numerical' or 'categorical'.
#' @param no_categories numeric; number of categories if categorical variables are required.
#' @param ... See details.
#' @details If 'numerical' is chosen, multivariate normal distribution is used (MASS::mvrnorm).
#' One can supply the mean vector and the covariance matrix using the parameter 'mu' and 'Sigma'. \cr\cr
#' If 'categorical' is chosen, a thresholded multivariate normal distribution is used (orddata::rmvord).
#' One can supply the covariance matrix with 'Sigma' and
#' the marginal probabilities for the categories for each covariate with 'marginal_probs'. \cr\cr
#' 'marginal_probs' expects a matrix.
#' For example, consider two covariates X1, X2, each covariate has two categories.
#' Suppose X1 has marginals (0.5, 0.5), and X2 has marginals (0.3, 0.7).
#' Then one should input "marginal_probs = matrix(c(0.5,0.5, 0.3,0.7), nrow = 2, byrow = FALSE)".
#' @export
generate_dependent_covariates <- function(no_pts, no_covariates, type = 'numerical',
                                          no_categories, ...) {
  if (!(type %in% c("numerical", "categorical")))
    stop("Wrong type. Must be one of 'numerical', 'categorical'.", call. = FALSE)
  if ((type == "categorical") & missing(no_categories))
    stop("'no_categories' must be available when type is categorical.", call. = FALSE)
  if (missing(type))
    print("parameter 'type' is not provided, type 'numerical' is assumed.")

  no_sim <- no_pts * no_covariates
  if (type == "numerical") {
    X_and_cor_mat <- generate_dnum_covariates(no_pts, no_covariates, ...)
  } else if (type == "categorical") {
    X_and_cor_mat <- generate_dcat_covariates(no_pts, no_covariates, no_categories, ...)
  }
  X <- X_and_cor_mat$X
  cor_mat <- X_and_cor_mat$correlation_matrix

  list(covariates = data.frame(X), correlation_matrix = cor_mat)
}
#[Core] Generate dependent numerical covariates
generate_dnum_covariates <- function(no_pts, no_covariates, mu, Sigma) {
  if (missing(mu)) mu <- rep(0, no_covariates)
  if (missing(Sigma))
    Sigma <- clusterGeneration::genPositiveDefMat(no_covariates)$Sigma
  cor_mat <- cov2cor(Sigma)

  list(X = MASS::mvrnorm(no_pts, mu, Sigma), correlation_matrix = cor_mat)
}
#[Core] Generate dependent categorical covariates
generate_dcat_covariates <- function(no_pts, no_covariates, no_categories,
                                     marginal_probs, cor_mat) {
  if (missing(cor_mat))
    cor_mat <- cov2cor(clusterGeneration::genPositiveDefMat(no_covariates)$Sigma)
  if (missing(marginal_probs))
    marginal_probs <- purrr::map(seq(no_covariates), ~discrete_uniform(no_categories))

  list(X = orddata::rmvord(n = no_pts, probs = marginal_probs, Cor = cor_mat),
       correlation_matrix = cor_mat)
}
#[Util] Generate discrete uniform pmf
discrete_uniform <- function(num_categories) {
  return(rep(1 / num_categories, num_categories))
}
