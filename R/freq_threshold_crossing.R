#' Frequency Estimation by Evaluating the Time Intervals between the arguments of succeeding local maxima
#'
#'
#' @param  x real valued array containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#'
#' @return a list with the three elements, estimated frequency, the standard deviation of the estimated frequency, and
#' the interquartile range of the estimated frequency.
#' @export
#'
#' @examples
#' x1 = sin(0.11*2*pi*(1:100));
#' x2 = sin(0.11*2*pi*(1:100))+ 1.01*sin(0.17*2*pi*(1:100));
#' freq_threshold_crossing(x1)
#' freq_threshold_crossing(x2)

freq_threshold_crossing <- function(x, delta_t = 1.0, thresh, ...)
{
  if (!is.vector(x) & !is.numeric(x) & any(is.na(x)) & any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) & length(delta_t) != 1L & any(is.infinite(delta_t))) stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0) stop("'delta_t' must be positive (>0)")
  if (var(x)== 0)
  {
    message("'x' does not contain fluctuations, returning NA");
    res = c(freq_threshold_crossing = NA_real_, sd_freq = NA_real_, IQR_freq = NA_real_, thresh = NA_real_)
  } else      if(missing(thresh))
  {

    maxmax <- peaks(x,span=7, strict=FALSE)
    minmin <- peaks(-x,span=7, strict=FALSE)
    x_max <- x[maxmax]
    x_min <- x[minmin]

    if(min(x_max) > max(x_min)) thresh <- 0.5*(min(x_max)+max(x_min)) else thresh <- mean(x)
  } else
  {

  xx <- x - thresh
  tt_0 <- which(xx[-c(length(xx))]<0 & xx[-c(1)]>0)
  if (length(tt_0)< 2)
    {
    message("'x' does not contain enough threshold crossings, returning NA");
    res <- c(freq_threshold_crossing = NA_real_, sd_freq = NA_real_, IQR_freq = NA_real_, thresh = NA_real_)
    }
  else
    {
      ISI_ZC <-  diff(tt_0)*delta_t
      n_ISI_ZC <- length(ISI_ZC)

      freq_TD <-  mean(1./ISI_ZC)
      freq_TD_sd <- sd(1./ISI_ZC)
      freq_TD_IQR <- IQR(1./ISI_ZC)

      res <- c(freq_threshold_crossing = freq_TD, sd_freq = freq_TD_sd, IQR_freq = freq_TD_IQR, thresh = thresh)

    }
  }
return(res)
}
