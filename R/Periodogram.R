#' Periodogram of a Real-Valued Time Series
#'
#' @param x real valued array containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#'
#' @return a list with the elements Frequency and Power
#' @export
#'
#' @examples
#' x1 <- rnorm(32)
#' Periodogram(x1)
#' Periodogram(x1, delta = 0.25)
#' Periodogram(x1, welch_window = FALSE)
#' x2 <- sin(0.1 * 2 * pi * (1:100));
#' plot(Periodogram(x2), type = 'l', co="orange")
#' points(Periodogram(x2), pch=21, cex=0.2)
#'
Periodogram <- function(x, delta_t = 1.0, welch_window = FALSE)
{
  if (!is.vector(x) | !is.numeric(x) | any(is.na(x)) | any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) | length(delta_t) != 1L | is.na(delta_t) | is.infinite(delta_t)) stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0) stop("'delta_t' must be positive (>0)")
  if (length(welch_window) != 1L | !is.logical(welch_window)) stop("'welch_window' must be logical of length one")


  N <- length(x)
  N_half <- floor(N/2)
  delta_f <- 1/(length(x)*delta_t)
  x_demean <- x - mean(x)
  if (welch_window) x_demean <- x_demean*welch(N)
  P <- abs(fft(x_demean)/N)
  P <- 2 * P[2: (1+ N_half)]^2
  P <- P /delta_f /N
  f <- delta_f * (1 : floor(N_half))

  res <- cbind(Frequency = f, Power = P)
  return(res)
}



welch <- function(N) {
  # N - length of the time series
  N_half  <-  floor(N/2)
  f <-  (1: N_half) - 0.5
  if (N %% 2 == 0) welch_window_function <- 1-(c(rev(f),f)/( N_half))^2.  else welch_window_function <- 1. - (c(rev(f),0,f)/N_half)^2.
  return( welch_window_function)

}


