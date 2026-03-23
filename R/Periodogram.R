#' Periodogram of a Real-Valued Time Series
#' @description This function computes the Periodogram, i.e., a simple estimator of the power spectral density of a real valued time series.
#' @param x real valued array containing the values of the time series
#' @param delta_t  a positive real value, the sampling time, default value is delta_t = 1.0
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `Frequency` -  a real valued array  containing the frequencies.
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
Periodogram <- function(x, delta_t = 1.0, welch_window = FALSE) {
  # ---- Input validation ----
  if (!is.numeric(x) || !is.vector(x) || anyNA(x) || any(!is.finite(x))) {
    stop("'x' must be a finite, non-missing numeric vector")
  }

  n <- length(x)
  if (n < 2L) {
    stop("'x' must contain at least two observations")
  }

  if (!is.numeric(delta_t) || length(delta_t) != 1L || !is.finite(delta_t) || delta_t <= 0) {
    stop("'delta_t' must be a single positive finite number")
  }

  if (!is.logical(welch_window) || length(welch_window) != 1L) {
    stop("'welch_window' must be a single logical value")
  }

  # ---- Preprocessing ----
  x_centered <- x - mean(x)

  if (welch_window) {
    w <- welch_window_fn(n)
    x_centered <- x_centered * w

    # Optional: normalize for window power (important for PSD correctness)
    U <- mean(w^2)
  } else {
    U <- 1
  }

  # ---- FFT ----
  X <- stats::fft(x_centered)

  # ---- One-sided PSD ----
  n_half <- floor(n / 2)
  freqs <- (0:n_half) / (n * delta_t)

  # Raw periodogram
  P <- (Mod(X)^2) * delta_t / (n * U)

  # One-sided correction (exclude DC and Nyquist handling)
  P <- P[1:(n_half + 1)]
  if (n > 2) {
    P[2:n_half] <- 2 * P[2:n_half]
  }

  # Remove DC component
  res <- list(
    Frequency = freqs[-1],
    PSD = P[-1]
  )

  return(res)
}


welch_window_fn <- function(n) {
  # Symmetric Welch window
  k <- seq_len(n) - 1
  m <- (n - 1) / 2
  w <- 1 - ((k - m) / m)^2
  return(w)
}
