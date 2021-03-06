% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features_util.R
\name{add_irrelevant_covariates}
\alias{add_irrelevant_covariates}
\title{A function to generate irrelevant covariates, numerical or categorical}
\usage{
add_irrelevant_covariates(df0, no_covariates, type, ...)
}
\arguments{
\item{df0}{dataframe; the data}

\item{no_covariates}{integer; number of irrelevant covariates to be added}

\item{type}{The type of the covariates; it must be 'numerical' or 'categorical'.}

\item{...}{Optional parameters to be passed to "generate_independent_covariates".}
}
\description{
A function to generate irrelevant covariates, numerical or categorical
}
\details{
This function calls 'generate_independent_covariates'.
See '?generate_independent_covariates' for details.
}

