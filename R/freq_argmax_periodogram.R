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
                                    welch_window = FALSE)
{
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(!is.finite(x)))
    stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L)
    stop("data series to short")
  if (length(delta_t) != 1L |
      !is.numeric(delta_t) |
      !is.finite(delta_t) |
      delta_t <= 0)
    stop("'delta_t' must be positive finite numeric of length one")
  if (length(welch_window) != 1L |
      is.na(welch_window) |
      is.infinite(welch_window) |
      !is.logical(welch_window))
    stop("'welch_window' must be logical of length one")
  if (length(f_min) > 1L |
      !is.numeric(f_min) |
      is.na(f_min) |
      !is.finite(f_min) |
      f_min < 0)
    stop("'f_min' must be must be positive numeric of length one")
  if (length(f_max) > 1L |
      !is.numeric(f_max) |
      is.na(f_max) |
      f_max == -Inf |
      f_max <= 0)
    stop("'f_max' must be must be positive numeric of length one")
  if (f_max < f_min )
    stop("'f_max' < 'f_min'")
  if (f_min >  1. / 2 / delta_t)
    stop("'f_min' is larger than the maximum of the sampled frequencies")
  if (f_max <  1 / length(x) / delta_t)
    stop("'f_max' is smaller than the minimum of the sampled frequencies")
  if (stats::var(x) == 0)
  {
    res <- list(freq_argmax_periodogram = NA_real_, Max = NA_real_ ,expl_var = NA_real_)
    message("time series 'x' does not contain fluctuations, returning NA")

  }
  else
  {
    x <- x - mean(x)
    P <- Periodogram(x, delta_t = delta_t, welch_window = welch_window)
    freq <- P$Frequency
    w <- which(freq >= f_min & freq <= f_max)
    if (length(w) == 0) stop("Frequency range does not contain sampled frequencies")
    if (length(w) < 4) warning("Frequency range contains less than 4 frequencies")
    freq <- freq[w]
    PP <- P$PSD[w]
    ww <- which.max(PP)
    argmax_freq <-  freq[(which.max(PP))]
    Max = max(PP)
    expl_var <- max(PP) / sum(PP)


    res <- list(freq_argmax_periodogram = argmax_freq, Max_P = Max , expl_var = expl_var)


  }
  return(res)
}
