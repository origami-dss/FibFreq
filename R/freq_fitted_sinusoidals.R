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

  ## ---------------------------
  ## Input validation
  ## ---------------------------
  stopifnot(
    is.numeric(x), is.vector(x), length(x) > 1, all(is.finite(x)),
    is.numeric(delta_t), length(delta_t) == 1, is.finite(delta_t), delta_t > 0,
    is.numeric(test_freqs), length(test_freqs) > 0, all(is.finite(test_freqs)),
    all(test_freqs > 0)
  )

  ## ---------------------------
  ## Precompute time vector
  ## ---------------------------
  n <- length(x)
  t <- seq_len(n) * delta_t
  var_x <- stats::var(x)

  if (var_x == 0) {
    warning("'x' has zero variance")
    return(list(
      freq_fitted = NA_real_,
      expl_var = NA_real_,
      params = c(offset = NA_real_, amp = NA_real_, phase = NA_real_),
      model_ts = rep(NA_real_, n)
    ))
  }

  ## ---------------------------
  ## Fast sinusoidal fit
  ## ---------------------------
  fit_one_freq <- function(freq) {
    s <- sin(2 * pi * freq * t)
    c <- cos(2 * pi * freq * t)

    # Design matrix
    X <- cbind(1, s, c)

    # Solve least squares via QR
    coef <- qr.solve(X, x)

    A <- coef[1]
    B_sin <- unname(coef[2])
    B_cos <- unname(coef[3])

    amp <- sqrt(B_sin^2 + B_cos^2)
    phase <- atan2(B_cos, B_sin)

    # residuals without refitting
    fitted <- X %*% coef
    resid <- x - fitted

    expl_var <- (var_x - stats::var(resid)) / var_x

   return( c(offset = A, amp = amp, phase = phase, expl_var = expl_var))
  }

  ## ---------------------------
  ## Evaluate all frequencies
  ## ---------------------------
  res_mat <- t(vapply(test_freqs, fit_one_freq, numeric(4)))

  ## ---------------------------
  ## Select best frequency
  ## ---------------------------
  best_idx <- which.max(res_mat[, "expl_var"])

  freq_fitted <- test_freqs[best_idx]
  params <- res_mat[best_idx, c("offset", "amp", "phase")]
  expl_var <- res_mat[best_idx, "expl_var"]

  ## ---------------------------
  ## Reconstruct model
  ## ---------------------------
  model_ts <- params["offset"] +
    params["amp"] * sin(2 * pi * freq_fitted * t + params["phase"])

  ## ---------------------------
  ## Output
  ## ---------------------------
  list(
    freq_fitted = freq_fitted,
    expl_var = unname(expl_var),
    params = params,
    model_ts = as.numeric(model_ts)
  )
}
