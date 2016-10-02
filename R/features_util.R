#' A function to generate irrelevant covariates, numerical or categorical
#' @param df0 dataframe; the data
#' @param no_covariates integer; number of irrelevant covariates to be added
#' @param type The type of the covariates; it must be 'numerical' or 'categorical'.
#' @param ... Optional parameters to be passed to "generate_independent_covariates".
#' @details This function calls 'generate_independent_covariates'.
#' See '?generate_independent_covariates' for details.
#' @export
add_irrelevant_covariates <- function(df0, no_covariates, type, ...) {
  #can change to dependent easily by converting to 'generate_dependent_covaraites' and
  #then subset the 'covariates' component.
  irrelevant_covariates <- generate_independent_covariates(nrow(df0), no_covariates, type, ...)
  names(irrelevant_covariates) <- paste( 'irr_', seq(no_covariates), sep = "" )
  dplyr::bind_cols(df0, irrelevant_covariates)
}
