% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features_dependent.R
\name{generate_dependent_covariates}
\alias{generate_dependent_covariates}
\title{Generate dependent covariates - numerical or (ordinal) categorical}
\usage{
generate_dependent_covariates(no_pts, no_covariates, type = "numerical",
  no_categories, ...)
}
\arguments{
\item{no_pts}{numeric; number of points to simulate.}

\item{no_covariates}{numeric; number of covariates required.}

\item{type}{character; 'numerical' or 'categorical'.}

\item{no_categories}{numeric; number of categories if categorical variables are required.}

\item{...}{See details.}
}
\description{
Generate dependent covariates - numerical or (ordinal) categorical
}
\details{
If 'numerical' is chosen, multivariate normal distribution is used (MASS::mvrnorm).
One can supply the mean vector and the covariance matrix using the parameter 'mu' and 'Sigma'. \cr\cr
If 'categorical' is chosen, a thresholded multivariate normal distribution is used (orddata::rmvord).
One can supply the covariance matrix with 'Sigma' and
the marginal probabilities for the categories for each covariate with 'marginal_probs'. \cr\cr
'marginal_probs' expects a matrix.
For example, consider two covariates X1, X2, each covariate has two categories.
Suppose X1 has marginals (0.5, 0.5), and X2 has marginals (0.3, 0.7).
Then one should input "marginal_probs = matrix(c(0.5,0.5, 0.3,0.7), nrow = 2, byrow = FALSE)".
}

