#' Argmax of the Periodogram
#'
#' @description
#' This function computes the argmax of the periodogram (with respect to a given frequency interval) for a time series,
#' i.e., the frequency which corresponds to the maximum peak in the periodogram in the considered frequency range.
#' This frequency is also known as fundamental or dominant frequency.
#'
#' @param x real valued array containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param f_min lower endpoint of the frequency interval
#' @param f_max upper endpoint of the frequency interval
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_argmax_periodogram` - the best-fitting frequency estimated with the Adapted Variable Period Technique.
#'   \item `Max_P` - the corresponding maximum value of the periodogram.
#'   \item `expl_var` -  the explained variance of the frequency component `freq_argmax_periodogram`.
#' }
#' @return   a named list containing
#'           (i) the argmax frequency,
#'           (ii) the maximum (i.e., the Periodogram at argmax) and
#'           (iii) the explained variance (i.e., the fraction of variance explained by the argmax frequency).
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. submitted to Frontiers in Signal Processing (2025)}

#' @examples
#' # Let's consider two synthetic time series and an example from the attached data set:
#' x1 <- sin( 0.05 * 2 * pi * (1:100))
#' x2 <- sin( 0.05 * 2 * pi * (1:100)) + 1.01 * sin(0.07 * 2 * pi * (1:100))
#' ecg_6  <- MiceFibECGs[,6]
#'
#' freq_argmax_periodogram(x1)
#' # Welch windowing can change the expected variance
#' freq_argmax_periodogram(x1, welch_window = TRUE)
#'
#' freq_argmax_periodogram(x2)
#' # The result can depend on the considered frequency
#' freq_argmax_periodogram(x2, f_min = 0.01, f_max = 0.06)
#'
#' freq_argmax_periodogram(ecg_6, delta_t = 0.001)
#'
freq_argmax_periodogram <- function(x,
                                    delta_t = 1.0,
                                    f_min = 0,
                                    f_max = Inf,
                                    welch_window = FALSE) {
  ## ---------------------------
  ## Input validation
  ## ---------------------------
  stopifnot(
    is.numeric(x),
    is.vector(x),
    length(x) > 1,
    all(is.finite(x)),
    is.numeric(delta_t),
    length(delta_t) == 1,
    is.finite(delta_t),
    delta_t > 0,
    is.logical(welch_window),
    length(welch_window) == 1,
    is.numeric(f_min),
    length(f_min) == 1,
    is.finite(f_min),
    f_min >= 0,
    is.numeric(f_max),
    length(f_max) == 1,
    f_max > 0
  )

  if (f_max < f_min)
    stop("'f_max' < 'f_min'")

  nyquist <- 1 / (2 * delta_t)
  if (f_min > nyquist)
    stop("'f_min' exceeds Nyquist frequency")

  ## ---------------------------
  ## Handle constant signal
  ## ---------------------------
  var_x <- stats::var(x)

  if (var_x == 0) {
    warning("'x' has zero variance")
    return(list(
      freq_argmax_periodogram = NA_real_,
      Max_P = NA_real_,
      expl_var = NA_real_
    ))
  }

  ## ---------------------------
  ## Compute periodogram
  ## ---------------------------
  x_centered <- x - mean(x)

  P <- Periodogram(x_centered, delta_t = delta_t, welch_window = welch_window)

  freq <- P$Frequency
  psd <- P$PSD

  ## ---------------------------
  ## Frequency filtering
  ## ---------------------------
  idx <- which(freq >= f_min & freq <= f_max)

  if (length(idx) == 0)
    stop("No sampled frequencies in given range")
  if (length(idx) < 4)
    warning("Frequency range contains < 4 points")

  freq_sub <- freq[idx]
  psd_sub <- psd[idx]

  ## ---------------------------
  ## Argmax
  ## ---------------------------
  best_idx <- which.max(psd_sub)

  freq_max <- freq_sub[best_idx]
  max_power <- psd_sub[best_idx]

  ## ---------------------------
  ## Explained variance (normalized power)
  ## ---------------------------
  expl_var <- max_power / sum(psd_sub)

  ## ---------------------------
  ## Output
  ## ---------------------------
  list(
    freq_argmax_periodogram = freq_max,
    Max_P = max_power,
    expl_var = expl_var
  )
}
