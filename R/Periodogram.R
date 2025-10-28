#' Periodogram of a Real-Valued Time Series
#' @description This function computes the Periodogram, i.e., a simple estimator of the power spectral density of a real valued time series.
#' @param x real valued array containing the values of the time series
#' @param delta_t  a positive real value, the sampling time, default value is delta_t = 1.0
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `Freqeuncy` -  a real valued array  containing the frequencies.
#'   \item `PSD` -  a real valued array containing the computed power spectral density.
#' }
#' @export
#'
#' @references{ Priestley, MB: *Spectral Analysis and Time series*. Academic Press, London (1981)}
#'
#' @examples
#' x1 <- rnorm(256)
#' P1 <- Periodogram(x1, delta = 0.25)
#' P1_w <-Periodogram(x1, delta_t = 0.25, welch_window = TRUE)
#' x2 <- sin(0.1 * 2 * pi * (1:100));
#' P2 <- Periodogram(x2)
#'
#' plot( P1$Frequency, P1$Power, type = 'l', col = "orange")
#' points(P1$Frequency, P1$Power, pch = 21, , bg = "black", cex = 0.5)
#' lines(P1_w$Frequency, P1_w$Power, cex = 0.2, col="violet")
#' points(P1_w$Frequency, P1_w$Power, pch = 25, bg = "blue", col="blue", cex = 0.5)
#'
#' plot( P2$Frequency, P2$Power, type = 'l', col = "orange")
#' points(P2$Frequency, P2$Power, pch = 21, bg = "black", cex = 0.2)
#'
Periodogram <- function(x, delta_t = 1.0, welch_window = FALSE)
{
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) |
      length(delta_t) != 1L |
      is.na(delta_t) |
      is.infinite(delta_t)) stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0) stop("'delta_t' must be positive (>0)")
  if (length(welch_window) != 1L |
      !is.logical(welch_window)) stop("'welch_window' must be logical of length one")


  N <- length(x)
  N_half <- floor(N/2)
  delta_f <- 1./delta_t
  x_demean <- x - mean(x)
  if (welch_window) x_demean <- x_demean*welch(N)
  P <- delta_t * abs(stats::fft(x_demean))
  P <- 2 * P[2: (1+ N_half)]^2
  P <- P /N /delta_t

  f <- delta_f * (1 : floor(N_half))/N

  res <- list(Frequency = f, PSD = P)
  return(res)
}



welch <- function(N) {
  # N - length of the time series

  N_half  <-  floor(N/2)
  f <-  (1: N_half) - 0.5
  if (N %% 2 == 0) welch_window_function <- 1-(c(rev(f),f)/( N_half))^2.  else welch_window_function <- 1. - (c(rev(f),0,f)/N_half)^2.

  return( welch_window_function)

}


