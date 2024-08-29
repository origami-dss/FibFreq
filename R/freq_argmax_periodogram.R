#' Argmax of the Periodogram
#'
#' @description
#' This function computes the argmax of the periodogram (with respect to a given frequency interval) for a time series,
#' i.e., the frequency which corresponds to the maximum peak in the periodogram in the considered frequency range.
#' This frequency is also known as dominant frequency.
#'
#' @param x real valued array containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param f_min lower endpoint of the frequency interval
#' @param f_max upper endpoint of the frequency interval
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#'
#' @return  a list containing the argmax frequency and the explained variance
#' (i.e., the fraction of variance explained by this specific frequency), and the used endpoints of the frequency interval
#'
#' @examples
#' x1 = sin(0.11*2*pi*(1:100));
#' x2 = sin(0.11*2*pi*(1:100))+ 1.01*sin(0.17*2*pi*(1:100));
#' x3 = sin(0.10*2*pi*(1:100))+ sin(0.20*2*pi*(1:100));
#' argmax_periodogram(x1)
#' argmax_periodogram(x2)
#' argmax_periodogram(x2, min_freq=0.05, max_freq = 0.15)
#' argmax_periodogram(x3)
freq_argmax_periodogram <- function(x, delta_t = 1.0, f_min = 0, f_max = Inf, welch_window = FALSE, names = TRUE)
{
  if(!is.vector(x) & !is.numeric(x) & any(is.na(x)) & any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) & length(delta_t) != 1L & any(is.infinite(delta_t))) stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0) stop("'delta_t' must be positive (>0)")
  if (length(welch_window) != 1L & !is.logical(welch_window)) stop("'welch_window' must be logical of length one")
  if (!is.numeric(f_min) & length(f_min) != 1L & f_min <=0)  stop("'f_min' must be must be positive numeric of length one")
  if (!is.numeric(f_max) & length(f_max) != 1L & f_max <=0)  stop("'f_max' must be must be positive numeric of length one")
  if (f_max <= f_min +1./length(x)/delta_t) stop("frequency range does not exist or does not contain a sampled frequency")
  if (!is.logical(names) & length(names) !=1L) stop("'names' must be logical of length one")
  if (var(x)== 0)
  {
    warning("'x' does not contain fluctuations, returning NA");
    res <- list(freq_argmax_periodogram = NA_real_, expressed_var = NA_real_, f_min = f_min, f_max = f_max)
  }
  else
  {

  P <- Periodogram(x,  delta_t = delta_t, welch_window = welch_window)
  freq <- P$Frequency
  w <- which(freq > f_min & freq < f_max)
  freq <- freq[w]
  PP <- P$Power[w]
  ww <- which.max(PP)
  argmax_freq <-  freq[(which.max(PP))]
  exp_var <- max(PP)/sum(PP)
  res <- c(freq_argmax_periodogram = argmax_freq, expressed_var = exp_var)

  if (names) names(res) = c("Argmax frequency ", "Expressed variance ")

  }
  return(res)
}

