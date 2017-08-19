#' A function to add irrelevant features to existing data
#' @param df0 dataframe; the data
#' @param p integer; number of irrelevant covariates to be added
#' @param distn One of 'gaussian', 'student-t', 'pareto', 'uniform', 'discrete_uniform', 'poisson', 'gamma'.
#' @param ... Optional parameters to be passed to "generate_features".
#' @details This function calls 'generate_features'.
#' See '?generate_features' for details.
#' @export
add_irrelevant_features <- function(df0, p, distn, ...) {
  #can change to dependent easily by converting to 'generate_dependent_covaraites' and
  #then subset the 'covariates' component.
  irrelevant_features <- generate_features(nrow(df0), p, distn, ...)
  names(irrelevant_features) <- paste( 'irr_', seq(p), sep = "" )
  dplyr::bind_cols(df0, irrelevant_features)
}
