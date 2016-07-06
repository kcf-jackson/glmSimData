#' Least-squared loss function
#' @export
ls_loss <- function(x, y) mean((x - y) ^ 2)


#' Partition a sequence into blocks
block_seq <- function(dim_param, block_num, block_size) {
  if (!missing(block_num) & !missing(block_size))
    stop("'block_num' and 'block_size' cannot both be specified.")
  if (!missing(block_num))
    return( block_seq_by_num(dim_param, block_num) )
  if (!missing(block_size))
    return( block_seq_by_size(dim_param, block_size) )
}
#' [Core] Partition a sequence into blocks given number of blocks
block_seq_by_num <- function(dim_param, block_num) {
  res <- unique(floor(seq(1, dim_param + 1, length.out = block_num)))
  res
}
#' [Core] Partition a sequence into blocks given block size
block_seq_by_size <- function(dim_param, block_size) {
  res <- seq(1, dim_param + 1, by = block_size)
  if (tail(res, 1) != dim_param + 1) res <- c(res, dim_param + 1)
  res
}


#' Stochastic Search
#' @export
stochastic_search <- function(dim_param, perf_fun,
                              loss_fun=ls_loss, target_perf,
                              max_iter=100, tol=0, curiosity=1,
                              block_num, block_size, param) {
  #initialisation
  if (!missing(block_num) & !missing(block_size))
    stop("'block_num' and 'block_size' cannot both be specified.")
  if (missing(param)) param <- rep(0, dim_param)
  current_perf <- perf_fun(param)
  current_loss <- loss_fun(current_perf, target_perf)
  cat("Beginning loss: ", current_loss, "\n")

  iter <- 0
  while ((iter < max_iter) & (current_loss > tol)) {
    iter <- iter + 1
    if (iter %% 10 == 0) print(iter)
    loop_seq <- block_seq(dim_param, block_num, block_size)
    for (i in head(seq_along(loop_seq), -1)) {
      new_param <- param
      update_range <- loop_seq[i]:(loop_seq[i+1] - 1)
      new_param[update_range] <- new_param[update_range] +
        curiosity * runif(length(update_range), min = -0.001, max = 0.001)
      new_perf <- perf_fun(new_param)
      new_loss <- loss_fun(new_perf, target_perf)
      if (new_loss < current_loss) {
        param <- new_param
        current_perf <- new_perf
        current_loss <- new_loss
        cat("Improved loss: ", current_loss, "\n")
      }
    }
    # cat("Current loss: ", current_loss, "\n")
  }
  cat("Final loss: ", current_loss, "\n")
  list(param, current_perf, current_loss)
}
