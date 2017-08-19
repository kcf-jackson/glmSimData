#' Generate dependent features - numerical or (ordinal) categorical
#' @param n integer; number of points to simulate.
#' @param p integer; number of covariates required.
#' @param type character; 'numerical' or 'categorical'.
#' @param num_categories numeric; number of categories if categorical variables are required.
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
generate_dependent_features <- function(n, p, type = 'numerical',
                                        num_categories = 5, ...) {
  if (missing(type))
    print("parameter 'type' is not provided, type 'numerical' is assumed.")
  if ((type == 'categorical') & (missing(num_categories)))
    print("parameter 'num_categories' is not provided, it is set to 5 by default.")
  if (!(type %in% c("numerical", "categorical")))
    stop("Wrong type. Must be one of 'numerical', 'categorical'.", call. = FALSE)

  if (type == "numerical") {
    X_and_cor_mat <- generate_dnum_covariates(n, p, ...)
  } else if (type == "categorical") {
    X_and_cor_mat <- generate_dcat_covariates(n, p, num_categories, ...)
  }
  X <- X_and_cor_mat$X
  cor_mat <- X_and_cor_mat$correlation_matrix

  list(covariates = data.frame(X), correlation_matrix = cor_mat)
}

#[Core] Generate dependent numerical covariates
generate_dnum_covariates <- function(n, p, mu, Sigma) {
  if (missing(mu)) mu <- rep(0, p)
  if (missing(Sigma)) Sigma <- clusterGeneration::genPositiveDefMat(p)$Sigma
  list(X = MASS::mvrnorm(n, mu, Sigma), correlation_matrix = cov2cor(Sigma))
}

#[Core] Generate dependent categorical covariates
generate_dcat_covariates <- function(n, p, num_categories,
                                     marginal_probs, Sigma) {
  if (missing(Sigma)) Sigma <- clusterGeneration::genPositiveDefMat(p)$Sigma
  cor_mat <- cov2cor(Sigma)

  if (missing(marginal_probs))
    marginal_probs <- purrr::map(seq(p), ~discrete_uniform(num_categories))

  list(X = orddata::rmvord(n = n, probs = marginal_probs, Cor = cor_mat),
       correlation_matrix = cor_mat)
}

#[Util] Generate discrete uniform pmf
discrete_uniform <- function(num_categories) {
  return(rep(1 / num_categories, num_categories))
}
