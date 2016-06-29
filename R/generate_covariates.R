#' Generate independent covariates - numerical or (ordinal) categorical
#' @param no_pts integer; number of data points to be simulated.
#' @param no_covariates integer; number of covariates.
#' @param type One of 'numerical' or 'categorical'.
#' @param no_categories integer; number of categories. Only applies when type = 'categorical'.
#' @param distn One of "gaussian", "student-t" or "pareto". Only applies when type = 'numerical'.
#' @param unit_var logical; if TRUE, then covariates are scaled to unit variance. Only applies when type = 'numerical'.
generate_independent_covariates <- function(no_pts, no_covariates,
                                            type = "numerical", no_categories, ...) {
  if (missing(type)) {
    type <- "numerical"
    print("parameter 'type' is not provided, type 'numerical' is assumed.")
  }
  if (!(type %in% c("numerical", "categorical")))
    stop("Wrong type. Must be one of 'numerical', 'categorical'.", call. = FALSE)
  if ((type == "categorical") & missing(no_categories))
    stop("'no_categories' must be available when type is categorical.", call. = FALSE)

  no_sim <- no_pts * no_covariates
  if (type == "numerical") {
    X <- generate_num_covariates(no_pts, no_covariates, ...)
  } else if (type == "categorical") {
    X <- matrix( sample(x = no_categories, size = no_sim, replace = TRUE, ...) - 1,
                 ncol = no_covariates, nrow = no_pts )
  }

  data.frame(X)
}
#[Core] Generate numerical independent covariates
generate_num_covariates <- function(no_pts, no_covariates,
                                    distn = "gaussian", ...) {
  rdist <- switch(distn,
    "gaussian" = rnorm,
    "student-t" = rt,
    "pareto" = VGAM::rpareto
    )
  no_sim <- no_pts * no_covariates
  X <- matrix( rdist(no_sim, ...), ncol = no_covariates, nrow = no_pts )
  data.frame(X)
}


#' Generate dependent covariates - numerical or ordinal
#' @param no_pts numeric; number of points to simulate.
#' @param no_covariates numeric; number of covariates required.
#' @param type character; 'numerical' or 'categorical'.
#' @param mu numeric vector; the mean of multivariate normal. Only applies when type = 'numerical'.
#' @param Sigma numeric matrix; the covariance matrix for multivariate normal / multivariate ordinal generation.
#' @param no_categories numeric; number of categories if categorical variables are required.
#' @param marginal_probs matrix; marginal probabilities of the categories for each covariates.
#' e.g. suppose no_categories = 2, X1 has marginals (0.5, 0.5), X2 has marginals (0.3, 0.7),
#' then one should input matrix(c(0.5,0.5, 0.3,0.7), nrow = 2, byrow = FALSE).
#' @param empirical_cov logical; if TRUE, the function also returns the covariance of
#' the simulated data.
#' @details Multivariate normal is used for dependent numerical covariates. And the package 'orddata'
#' is used to generate dependent multivariate ordinal variables.
generate_dependent_covariates <- function(no_pts, no_covariates, type = 'numerical',
                                          mu, Sigma, no_categories, marginal_probs,
                                          empirical_cor = FALSE) {
  if (missing(type)) {
    type <- "numerical"
    print("parameter 'type' is not provided, type 'numerical' is assumed.")
  }
  if (!(type %in% c("numerical", "categorical")))
    stop("Wrong type. Must be one of 'numerical', 'categorical'.", call. = FALSE)
  if ((type == "categorical") & missing(no_categories))
    stop("'no_categories' must be available when type is categorical.", call. = FALSE)
  if (missing(Sigma))
    Sigma <- clusterGeneration::genPositiveDefMat(no_covariates)$Sigma
  cor_mat <- cov2cor(Sigma)

  no_sim <- no_pts * no_covariates
  if (type == "numerical") {
    if (missing(mu)) mu <- rep(0, no_covariates)
    X <- MASS::mvrnorm(no_pts, mu, Sigma)
  } else if (type == "categorical") {
    if (missing(marginal_probs))
      marginal_probs <- purrr::map(seq(no_covariates),
                                   ~discrete_uniform(no_categories))
    X <- orddata::rmvord(n = no_pts, probs = marginal_probs, Cor = cor_mat)
  }

  if (empirical_cor)
    return(list(covariates = data.frame(X), correlation_matrix = cor_mat,
                empirical_correlation = cov2cor(cov(X))))
  list(covariates = data.frame(X), correlation_matrix = cor_mat)
}


#' A function to generate irrelevant covariates, numerical or categorical
#' @param df0 dataframe; the data
#' @param no_covariates integer; number of irrelevant covariates to be added
#' @export
add_irrelevant_covariates <- function(df0, no_covariates, type, ...) {
  #can change to dependent easily by converting to 'generate_dependent_covaraites' and
  #then subset the 'covariates' component.
  irrelevant_covariates <- generate_independent_covariates(nrow(df0), no_covariates, type, ...)
  names(irrelevant_covariates) <- paste( 'irr_', seq(no_covariates), sep = "" )
  dplyr::bind_cols(df0, irrelevant_covariates)
}


discrete_uniform <- function(num_categories) {
  return(rep(1 / num_categories, num_categories))
}
