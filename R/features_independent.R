#' Generate independent covariates - numerical or (ordinal) categorical
#' @param no_pts integer; number of data points to be simulated.
#' @param no_covariates integer; number of covariates.
#' @param type One of 'numerical' or 'categorical'.
#' @param no_categories integer; number of categories. Only applies when type = 'categorical'.
#' @param ... See details.
#' @details If 'numerical' is chosen, one can specify the distribution through parameter 'distn'.
#' The choices are "gaussian", "student-t" or "pareto".
#' One should also provide the parameters when one specifies the distribution.
#' For more details, see "?rnorm", "?rt", "?VGAM::rpareto".
#' Note that for 'gaussian', standard normal is assumed if parameters are not given. \cr\cr
#' If 'categorical' is chosen, one can specify the distribution for the categories through the
#' parameter 'prob'.
generate_independent_covariates <- function(no_pts, no_covariates, type = "numerical",
                                            no_categories, ...) {
  if (!(type %in% c("numerical", "categorical")))
    stop("Wrong type. Must be one of 'numerical', 'categorical'.", call. = FALSE)
  if ((type == "categorical") & missing(no_categories))
    stop("'no_categories' must be available when type is categorical.", call. = FALSE)
  if (missing(type))
    print("Parameter 'type' is not provided, type 'numerical' is assumed.")

  no_sim <- no_pts * no_covariates
  if (type == "numerical") {
    X <- generate_num_covariates(no_pts, no_covariates, ...)
  } else if (type == "categorical") {
    X <- generate_cat_covariates(no_pts, no_covariates, no_categories, ...)
  }

  data.frame(X)
}
#[Core] Generate independent numerical covariates
generate_num_covariates <- function(no_pts, no_covariates, distn = "gaussian", ...) {
  rdist <- switch(distn,
    "gaussian" = rnorm,
    "student-t" = rt,
    "pareto" = VGAM::rpareto
    )
  no_sim <- no_pts * no_covariates
  X <- matrix( rdist(no_sim, ...), ncol = no_covariates, nrow = no_pts )
  data.frame(X)
}
#[Core] Generate independent categorical covariates
generate_cat_covariates <- function(no_pts, no_covariates, no_categories, ...) {
  no_sim <- no_pts * no_covariates
  matrix( sample(x = no_categories, size = no_sim, replace = TRUE, ...) - 1,
          ncol = no_covariates, nrow = no_pts )
}
