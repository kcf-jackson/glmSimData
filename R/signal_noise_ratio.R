#' Initialise objective function for optimisation
init_signal_noise <- function(my_data, family, tf = identity) {
  function(param) {
    glm_model <- glm(resp_var ~ . -1, data = my_data, family, weights = tf(param))
    extract_ratio(glm_model)
  }
}


#' extract signal-noise ratio from GLM object.
#' @param model_obj 'glm' object.
#' @param absolute boolean; if FALSE, return the signed ratio. 
#' @details The signal-noise ratio must be non-negative, but sometimes the 
#' signs are needed for analysis. 
#' @export
extract_ratio <- function(model_obj, absolute = TRUE) {
  coeff <- summary(model_obj)$coefficients
  r <- coeff[,1] / coeff[,2]
  if (absolute) return(abs(r))
  r
}


#' Compare the signal-noise ratios
#' @param r1 numeric vector; usually the sample signal-noise ratio.
#' @param r2 numeric vector; usually the target signal-noise ratio.
#' @export
diff_ratio <- function(r1, r2) {
  print(rbind(simulated_ratio = r1, target_ratio = r2))
  l1 <- vec_find_signal_label(r1)
  l2 <- vec_find_signal_label(r2)
  print(rbind(simulated_label = l1, target_label = l2))
  cat("The loss is: ", ls_loss(r1, r2), "\n")
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
#' @param signal_noise_ratio numeric vector; 
#' @return character vector; the labels.
#' @export
vec_find_signal_label <- Vectorize(find_signal_label)  #Vectorize


#' Generate some random labels
#' @param num_label integer; number of labels desired.
#' @param labels vector; labels to be drawn from.
#' @return character vector; the labels.
#' @export
random_labels <- function(num_label, labels = c("S", "M", "L")) {
  sample(labels, num_label, replace = TRUE)
}
