#' Initialise objective function for optimisation
#' @export
init_signal_noise <- function(y, X, family, tf = identity) {
  my_data <- data.frame(resp = y, X)
  function(param) {
    tmp <- summary(glm(resp ~ . -1, data = my_data, 
                        family, weights = tf(param)))
    abs(tmp$coefficients[,1] / tmp$coefficients[,2])
  }
}


init_threshold_L1_reg <- function(threshold, tf = identity) {
  function(param) {
    param <- tf(param)
    abs(L1_reg(param) - threshold)
  }
}


#' Categorise signal-noise ratio
find_signal_label <- function(signal_noise_ratio) {
  # <--XS-- 0.22 --S-- 0.61 --M-- 1.65 --L-- 4.48 --XL-->
  # SN_intervals <- c(0.22, 0.61, 1.65, 4.48)  #on exponential scale
  # SN_labels <- c("XS", "S", "M", "L", "XL")
  # <--S-- 1.65 --M-- 4.48 --L-->
  SN_intervals <- c(1.65, 4.48)  #on exponential scale
  SN_labels <- c("S", "M", "L")
  for (i in seq_along(SN_intervals)) {
    if (signal_noise_ratio < SN_intervals[i])
      return(SN_labels[i])
  }
  tail(SN_labels, 1)
}
#' Categorise signal-noise ratio (vectorised)
#' @export
vec_find_signal_label <- Vectorize(find_signal_label)  #Vectorize


#' Generate some random labels
#' @export
random_labels <- function(num_label, labels = c("S", "M", "L")) {
  sample(labels, num_label, replace = TRUE)
}
