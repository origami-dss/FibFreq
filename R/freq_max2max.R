#' Frequency Estimation by Evaluating the Max-to-Max Time Intervals
#'
#' @param  x real valued array containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param ...  arguments to be passed to the function splus2R::peaks. Here span = 11, strict = TRUE and endbehavior = 0 are used as default values.
#'
#' @return a vector with the estimated frequency, its standard deviation and its interquartile range

#' @examples
#' x1 = sin(0.11*2*pi*(1:100));
#' x2 = sin(0.11*2*pi*(1:100))+ 1.01*sin(0.17*2*pi*(1:100));
#' freq_max2max(x1)
#' freq_max2max(x2)
#' freq_max2max(sunspots, delta_t=1./12, span = 51)

freq_max2max <- function(x, delta_t = 1.0, ... )
{
  if (!is.vector(x) & !is.numeric(x) & any(is.na(x)) & any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) & length(delta_t) != 1L & any(is.infinite(delta_t))) stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0) stop("'delta_t' must be positive (>0)")

# transfer the arguments for splus2R::peaks function
  args <- list(...)

  if (!"span" %in% names(args)) {
    args$span <- 11
  }
  if (!"strict" %in% names(args)) {
    args$strict <- TRUE
  }
  if (!"endbehavior" %in% names(args)) {
    args$endbehavior <- 0
  }
  args$x <- x
€
  maxmax <- do.call(splus2R::peaks, args)

  n_max <- length(maxmax[ maxmax == TRUE])
  if (n_max < 3)
    {
    warning("at least three identified maxima are required, returning NA")
    res <- list(freq_max2max = NA_real_, freq_max2max_sd = NA_real_, freq_max2max_IQR = NA_real_, n_max = n_max )
    }
  else
    {
    ISI_maxmax <-  diff(which(maxmax))*delta_t
    n_ISI_maxmax <- length(ISI_maxmax)

    freq_max2max <-  mean(1./ISI_maxmax)
    freq_max2max_sd <- sd(1./ISI_maxmax)
    freq_max2max_IQR <- IQR(1./ISI_maxmax)

    res = list(freq_max2max = freq_max2max, freq_max2max_sd = freq_max2max_sd, freq_max2max_IQR = freq_max2max_IQR, n_max = n_max )
    }
  return(res)
}

