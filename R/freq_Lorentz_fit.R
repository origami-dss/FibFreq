#' Frequency Estimation Using a Lorentz Fit to the Amplitude Spectrum
#'
#' @description
#' This function is fitting a Lorentzian to the Fourier amplitudes of the given time series.
#' The parameters of the Lorentzian are returned, as well as the Lorentzian model.
#' The Levenberg-Marquardt  algorithm provided by the gslnls-library  is applied for performing the nonlinear least squares fit.
#' A multi-start algorithm is used with the ranges of the start parameters set to the minimimum and maximum frequency of the Fourier amplitudes
#' following the given parameters f_min and f_max.
#'
#'
#' @param  x real valued array containing the values of the time series
#' @param delta_t  a positive real value, the sampling time, default value is delta_t = 1.0
#' @param f_min  a positive real value, the lower bound of the considered frequency interval
#' @param f_max  a positive real value, the upper bound of the considered frequency interval
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#' @param control 	an optional list of control settings for the nonlinear least-squares algorithm (i.e., the function nls).
#' See \link[stats]{nls.control} for the names of the settable control values and their effect.
#' @param algorithm  character string specifying the algorithm to use. The default is "lm".  See   \link[stats]{nls} for other options.
#'
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_Lorentz` - the  center of the fitted Lorentzian.
#'   \item `scale_Lorentz` - the scale parameter of the fitted Lorentzian.
#'   \item `max_Lorentz_fit` - the  maximum value of the fitted Lorentzian.
#'   \item `expl_var` -  the  explained variance of the fitted Lorentzian.
#'   \item `Lorentz_model` -  a named list with the amplitude spectrum and the fitted model.
#'   \item `WARN` -  warning of the gslnls::gsl_nls - algorithm, NULL in case of no warning.
#' }
#'
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms. submitted to Frontiers in Signal Processing (2025)}
#' @references{ M. Galassi et al., GNU Scientific Library Reference Manual (3rd Ed.), 2009, ISBN 0954612078. }
#'
#' @examples
#' x1 = sin(0.11 * 2*pi *(1:100))
#' x2 = sin(0.11 * 2*pi *(1:100)) + 1.01 * sin(0.17 * 2*pi * (1:100))
#' ecg_6  <- MiceFibECGs[,6]
#' freq_Lorentz_fit(x1)
#' # For the following few examples we will focus on the fitted parameters and the explained variance
#' freq_Lorentz_fit(x2)[1:4]
#' # Welch windowing can change the Lorentz fit
#' freq_Lorentz_fit(x2, welch_window = TRUE)[1:4]
#' # A narrower (i.e., too narrow) frequency interval is changing the estimated
#' # frequency and the explained variance
#' freq_Lorentz_fit(x2, f_min = 0.02, f_max = 0.15, welch_window = TRUE)[1:4]
#'
#' # In case of bandpass filtered times series, the frequency range
#' # for the Lorentz fit should be contained in the frequency range of the signal.
#' res <- freq_Lorentz_fit(ecg_6, delta_t = 0.001, f_min = 1, f_max = 45)
#' res[1:2]
#'
#'
#' # Vizualization of the fitted Lorentz model:
#' xlab = "Frequency (Hz)"
#' ylab = "Fourier Amplitude"
#' plot(res$Lorentz_model$Frequency, res$Lorentz_model$Fourier_Amps, xlab = xlab, ylab = ylab, pch = 3)
#' lines(res$Lorentz_model$Frequency, res$Lorentz_model$modelled_Fourier_Amps, col = "red")
#'
#' legend("topright" , c("Fourier Amplitudes", "Lorentz Fit"),
#' col = c("black","red"), lty = c(0,1), pch = c(3,NA))
#'

freq_Lorentz_fit <- function(x,
                             delta_t = 1.0,
                             welch_window = FALSE,
                             f_min = 0,
                             f_max = Inf,
                             control = gslnls::gsl_nls_control(
                               maxiter = 500,
                               ftol = 1e-5,
                               gtol = 1e-5,
                               xtol = 1e-5,
                               h_df = 1e-5
                             ),
                             algorithm = "lm") {
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
    delta_t > 0,
    is.finite(delta_t),
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
  if (stats::var(x) == 0) {
    warning("'x' has zero variance")
    return(
      list(
        freq_Lorentz = NA_real_,
        scale_Lorentz = NA_real_,
        alpha_Lorentz = NA_real_,
        expl_var = NA_real_,
        Lorentz_model = NULL,
        WARN = NULL
      )
    )
  }

  ## ---------------------------
  ## FFT and preprocessing
  ## ---------------------------
  N <- length(x)
  N_half <- floor(N / 2)
  delta_f <- 1 / (N * delta_t)

  x_centered <- x - mean(x)
  if (welch_window) {
    x_centered <- x_centered * welch_window_fn(N)
  }

  fft_vals <- stats::fft(x_centered)
  Amp <- 2 * abs(fft_vals[2:(N_half + 1)]) / N
  freq <- delta_f * seq_len(N_half)

  ## ---------------------------
  ## Frequency windowing
  ## ---------------------------
  idx <- which(freq >= f_min & freq <= f_max)

  if (length(idx) < 4)
    stop("Frequency range contains < 4 points")
  if (length(idx) < 10)
    warning("Fitting on < 10 points may be unstable")

  freq <- freq[idx]
  Amp <- Amp[idx]

  ## ---------------------------
  ## Initial parameter estimates
  ## ---------------------------
  w_max <- which.max(Amp)
  mean_freq <- weighted.mean(freq, Amp)
  sigma_start <- sqrt(weighted.mean((freq - mean_freq)^2, Amp))
  freq_max = freq[w_max]
  data_fit <- data.frame(freq = freq, Amp = Amp)

  ## ---------------------------
  ## Fit
  ## ---------------------------


  form <- Amp ~ FibFreq:::Lorentz_fn_with_jacobian(freq, alpha_Lorentz, Lorentz_freq, sigma)
  environment(form) <- environment()

  fit <- withWarnings(
    gslnls::gsl_nls(
      fn = form,
      data = data_fit,
      start = list(
        alpha_Lorentz = 1,
        Lorentz_freq = freq_max,
        sigma = sigma_start
      ),
      control = control,
      algorithm = algorithm
    )
  )

  ## ---------------------------
  ## Extract results
  ## ---------------------------
  fitted_model <- fit$value
  params <- fitted_model$m$getPars()

  pred <- stats::predict(fitted_model)

  expl_var <- sum(pred^2) / sum(Amp^2)

  ## ---------------------------
  ## Output
  ## ---------------------------
  list(
    freq_Lorentz = unname(params["Lorentz_freq"]),
    scale_Lorentz = unname(params["sigma"]),
    alpha_Lorentz = unname(params["alpha_Lorentz"]),
    expl_var = expl_var,
    Lorentz_model = list(
      Frequency = freq,
      Fourier_Amps = Amp,
      modelled_Fourier_Amps = pred
    ),
    WARN = fit$warnings
  )
}



# Auxiliary Function

# author Luke Tierney (2004), R-help post
#	https://stat.ethz.ch/pipermail/r-help/2004-June/052132.html

withWarnings <- function(expr) {
  # Use a local environment to store warnings to avoid repeated <<- copying
  # However, for simply returning a list of objects, this is more robust:
  local_vars <- new.env()
  local_vars$W <- list()



  wHandler <- function(w) {
    local_vars$W <- c(local_vars$W, list(w))
    invokeRestart("muffleWarning")
  }

  val <- withCallingHandlers(expr, warning = wHandler)

  # Return structured output
  list(value = val, warnings = local_vars$W)
}


## ---------------------------
## Lorentz model
## ---------------------------
Lorentz_fn <- function(freq,
                       alpha_Lorentz,
                       Lorentz_freq,
                       sigma) {
  (alpha_Lorentz / pi) * sigma /
    ((freq - Lorentz_freq)^2 + sigma^2)
}

Lorentz_fn_with_jacobian <- function(freq, alpha_Lorentz, Lorentz_freq, sigma) {
  D <- (freq - Lorentz_freq)^2 + sigma^2
  fit <- (alpha_Lorentz / pi) * sigma / D

  jacobian <- cbind(
    alpha_Lorentz = (1 / pi) * sigma / D,
    Lorentz_freq = (2 * alpha_Lorentz * sigma * (freq - Lorentz_freq)) / (pi * D^2),
    sigma = (alpha_Lorentz / pi) * ((freq - Lorentz_freq)^2 - sigma^2) / (D^2)
  )

  attr(fit, "gradient") <- jacobian

  return(fit)
}


