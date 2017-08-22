#' A package for simulating data from a GLM model
#' @description This package simulates data following a GLM model with dependent / independent, numerical / ordinal covariates.
#' @docType package
#' @name glmSimData
#' @author Jackson Kwok
NULL

#' Import functions from the packages.
#' @name Imported functions
#' @keywords internal
#' @importFrom stats cov2cor gaussian glm model.matrix rbinom rexp rnbinom rgamma
#' rnorm rpois rt runif var
#' @importFrom utils head tail
#' @importFrom magrittr %>% %<>%
NULL

utils::globalVariables(c("."))
