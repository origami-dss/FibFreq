#' Frequency Estimation Using Fitted Sinusoidal Functions for a Set of Frequencies
#'
#' @description This function is determining the fundamental frequency by
#' identifying the best fitting sinusoidal function with respect to a given
#' range of frequencies. It identifies the frequency that corresponds to
#' the maximum explained variance.
#' @param x        a real valued array containing the values of the time series
#' @param delta_t  a positive real value, the sampling time, default value is delta_t = 1.0
#' @param test_freqs a real-valued vector with positive values, the test frequencies
#'
#'@return  a named list containing the following components:
#' \itemize{
#'   \item `freq_fitted` - the best-fitting frequency.
#'   \item `expl_var` -  the explained variance of the frequency component `freq_fitted`.
#'   \item `params` - the model parameters for offset + amp * sin (2*pi*freq + phase).
#'   \item `model_ts` -  the fitted model.
#' }
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. submitted to Frontiers in Signal Processing (2025)}

#'
#' @examples
#' # Let's consider three synthetic time series and an example from the attached data set:
#' x1 <- sin( 0.05 * 2 * pi * (1 : 100))
#' x2 <- x1 + 1.01 * sin( 0.07 * 2 * pi * (1:100))
#' x3 <- x1 + 1.0 * sin( 0.07 * 2 * pi * (1:100))
#' ecg_6  <- MiceFibECGs[,6]
#'
#' freq_fitted_sinusoidals(x1, test_freqs = seq(0.03, 0.20, by = 0.01))
#' freq_fitted_sinusoidals(x2, test_freqs = seq(0.03, 0.20, by = 0.01))
#' # The result can depend on the considered set of frequencies
#' freq_fitted_sinusoidals(x2, test_freqs = seq(0.02, 0.06, by = 0.01))
#' freq_fitted_sinusoidals(x2,  test_freqs = runif(100))
#' freq_fitted_sinusoidals(x3,  test_freqs = seq(0.03, 0.20, by = 0.01))
#'
#' res <- freq_fitted_sinusoidals(ecg_6,  test_freqs = 1:30, delta_t = 0.001)
#' print(c("freq_fitted:",res$freq_fitted))
#' print(c("expl_var:",round(res$expl_var, 2)))
#' # Let's plot the fitted model
#'
#' xlab = "Time [ms]"
#' ylab = "ECG"
#' plot(1:1000, ecg_6, xlab = xlab, ylab = ylab, "l", lwd = 1.5)
#' lines(1:1000, res$model, col = "red", lty = 2)


freq_fitted_sinusoidals <- function(x, delta_t = 1.0, test_freqs) {
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(is.infinite(x)))
    stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L)
    stop("data series to short")
  if (length(delta_t) != 1L |
      !is.numeric(delta_t) |
      is.infinite(delta_t) |
      delta_t <= 0)
    stop("'delta_t' must be positive finite numeric of length one")
  if (!is.vector(test_freqs)|
      length(test_freqs) == 0 |
      !is.numeric(test_freqs) |
      any(is.na(test_freqs)) |
      any(is.infinite(test_freqs)) |
      any(test_freqs <= 0))
    stop("'test_freqs' must be non-infinite real-valued  numeric vector with positive elements")

    l <- length(test_freqs)

    t(sapply(test_freqs,fit_sinusoidal_function, x = x, delta_t = delta_t)) -> res

#    second_res <- cbind(unname(test_freqs), unname(first_res[,4]))

    w <- which.max(res[,4])

    freq_fitted =  test_freqs[w]
    expl_var = unname(res[w,4])
    params = res[w,1:3]
    model_ts =  params[1] + params[2] * sin (2*pi * freq_fitted *(1:length(x))*delta_t + params[3])

    res <- list(freq_fitted = freq_fitted, expl_var = expl_var, params = params, model_ts = model_ts)
  return(res)
}


fit_sinusoidal_function <- function(x, delta_t, freq )
{
  {
    t <- (1:length(x)) * delta_t
    res_lm <- stats::lm(x ~ sin(2 * pi * freq * t) + cos(2 * pi * freq * t))
    A <- res_lm$coefficients[[1]]
    B <- sqrt(res_lm$coefficients[[2]]^2 + res_lm$coefficients[[3]]^2)
    C <- atan2(res_lm$coefficients[[3]],res_lm$coefficients[[2]])
    expl_var <- (stats::var(x)- stats::var(res_lm$residuals)) / stats::var(x)
    res <- c(offset = A, amp = B, phase = C, expl_var = expl_var)

  }
  return(res)
}



