# Initialise objective function for optimisation
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
  l1 <- find_signal_label(r1)
  l2 <- find_signal_label(r2)
  print(rbind(simulated_label = l1, target_label = l2))
  cat("The loss is: ", ls_loss(r1, r2), "\n")
}


# Categorise signal-noise ratio
atomic_find_signal_label <- function(signal_noise_ratio) {
  # <--XS-- 0.22 --S-- 0.61 --M-- 1.65 --L-- 4.48 --XL-->
  # SN_intervals <- c(0.22, 0.61, 1.65, 4.48)  #on exponential scale
  # SN_labels <- c("XS", "S", "M", "L", "XL")
  # <--S-- 1.65 --M-- 4.48 --L-->
  SN_intervals <- exp(c(0.5, 1.5))
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
find_signal_label <- Vectorize(atomic_find_signal_label)  #Vectorize


#' Generate some random labels
#' @param num_label integer; number of labels desired.
#' @param labels vector; labels to be drawn from.
#' @return character vector; the labels.
#' @export
random_labels <- function(num_label, labels = c("S", "M", "L")) {
  sample(labels, num_label, replace = TRUE)
}


#' Generate some random signals based on labels
#' @description Given a label, the function draws a number from the interval
#' corresponding to the label.
#' @param labels vector; the desired labels
#' @param labels_table dataframe; a table mapping labels to intervals.
#' See `create_labels_table()` for an example.
#' @examples
#' labels_table <- create_labels_table()
#' random_signals(c(rep("S", 4), rep("M", 3)), labels_table)
#' @export
random_signals <- function(labels, labels_table) {
  if (!all(labels %in% labels_table$labels))
    stop("Not all labels are mapped to an interval.")
  purrr::map_dbl(labels, ~draw_signal(.x, labels_table))
}
draw_signal <- function(label, labels_table) {
  ind <- which(labels_table$labels == label)
  mapping <- labels_table[ind, ]
  if (mapping$max == Inf)
    return(mapping$min + rexp(1))
  runif(1, min = mapping$min, max = mapping$max)
}


#' Create a dataframe mapping labels to intervals.
#' @param labels vector; the labels.
#' @param partition vector; the breakpoint partitioning the positive real line.
#' @export
create_labels_table <- function(labels = c("S", "M", "L"),
                                partition = c(0, exp(c(0.5, 1.5)), Inf)) {
  labels <- unique(labels)
  data.frame(labels = labels,
             min = head(partition, -1),
             max = tail(partition, -1))
}
