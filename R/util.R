#' Sum of matrix columns weighted by a vector
#' @param m1 a matrix
#' @param v1 a vector
#' @return a vector
#' @examples
#' m0 <- matrix(1:9, nrow = 3); m0
#' v0 <- c(0.2, 0.2, 0.9); v0
#' matrix_times_vector( m0, v0 )
matrix_times_vector <- function(m1, v1){
  cum_sum <- numeric(nrow(m1))
  for (i in seq_along(v1)){
    cum_sum <- cum_sum + m1[,i] * v1[i]
  }
  cum_sum
}


#' extension of expand.grid to 2 lists
expand.lists <- function(df0, df1) {
  res <- c()
  for (i in 1:nrow(df0)) {
    res <- rbind(res, cbind(df0[i,], df1))
  }
  res
}


#' Take exponential then round a number
#' @description It's used to guarantee non-negative integers.
round_exp <- function(x) round(exp(x)) 


#' Take exponential then round a number
#' @description It's used to guarantee non-negative integers.
ceil_exp <- function(x) ceiling(exp(x)) 


#' Trim the first s significant digits.
#' @export
trim_big_number <- function(n, s = 2) {
  trim_index <-  ceiling(log(n, 10)) - s
  n %% (10 ^ trim_index)
}
