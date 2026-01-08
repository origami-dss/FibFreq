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
                             control = gslnls::gsl_nls_control(maxiter = 500, ftol = 0.00001, gtol = 0.00001, xtol = 0.00001, h_df = 0.00001),
                             algorithm = "lm")
{
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(!is.finite(x)))
    stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L)
    stop("data series to short")
  if (!is.numeric(delta_t) |
      length(delta_t) != 1L |
      any(!is.finite(delta_t)))
    stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0)
    stop("'delta_t' must be positive (>0)")
  if (length(welch_window) != 1L |
      !is.logical(welch_window))
    stop("'welch_window' must be logical of length one")
  if (!is.numeric(f_min) |
      length(f_min) != 1L |
      !is.finite(f_min) |
      f_min < 0)
    stop("'f_min' must be must be positive numeric of length one")
  if (!is.numeric(f_max) |
      length(f_max) != 1L |
      f_max == -Inf |
      f_max <= 0)
    stop("'f_max' must be must be positive numeric of length one")
  if (f_max < f_min)
    stop("'f_max' < 'f_min'")
  if (f_min >  1. / 2 / delta_t)
    stop("'f_min' is larger than the maximum of the sampled frequencies")
  if (f_max <  1 / length(x) / delta_t)
    stop("'f_max' is smaller than the minimum of the sampled frequencies")
  if (stats::var(x) == 0)
  {
    warning("'x' does not contain fluctuations, returning NA")

    res <- list(
      freq_Lorentz = NA_real_,
      scale_Lorentz = NA_real_,
      alpha_Lorentz = NA_real_,
      expl_var = NA_real_,
      Lorentz_model = NULL,
      WARN = NULL
    )
  }
  else
  {
    N <- length(x)
    N_half <- floor(N / 2)
    delta_f <- 1 / (length(x) * delta_t)
    x_demean <- x - mean(x)
    if (welch_window)
      x_demean <- x_demean * welch(N)

    A <- abs(stats::fft(x_demean) / N)
    A <- 2 * A[2:(1 + N_half)]
    freq <- delta_f * (1:N_half)


    w <- which (freq >= f_min & freq <= f_max)
    if (length(w) < 4)
      stop("Frequency range contains less than 4 frequencies")
    if (length(w) < 10)
      warning("The Lorentzian is fitted to < 10 data points")
    freq <- freq[w]
    Amp <- A[w]
    PP <- data.frame(freq = freq, Amp = Amp)

    ww <- which.max(Amp)
    argmax_freq <-  freq[ww[1]]
    mean_freq <-  sum(Amp * freq) / sum(Amp)
    sigma_squared <- sum(Amp * (freq - mean_freq)^2) / sum(Amp)
    sigma_start <- sqrt(sigma_squared)



    result_fitting <- withWarnings(
      gslnls::gsl_nls(
            Amp ~  (alpha_Lorentz/pi)* sigma / ((freq - Lorentz_freq)^2 + sigma^2),
            data = PP,
            start = list( alpha_Lorentz = 1, Lorentz_freq = c(min(freq),max(freq)), sigma = sigma_start),
            jac = function(par) with(as.list(par),                 ## jacobian
            cbind( (1/pi)* sigma / ((freq - Lorentz_freq)^2 + sigma^2), Lorentz_freq = (alpha_Lorentz/pi) * 2 * sigma * (freq - Lorentz_freq)/((freq - Lorentz_freq)^2 + sigma^2)^2,  sigma =  (alpha_Lorentz/pi) * ((freq - Lorentz_freq)^2 - sigma^2)/((freq - Lorentz_freq)^2 + sigma^2)^2)
            ),
            control = control,
            algorithm = "lm")
    )


     Fourier_Amp_model <- stats::predict(result_fitting$value)
     fitted_params <- result_fitting$value$m$getPars()
     freq_Lorentz <-  unname(fitted_params ["Lorentz_freq"])
     scale_Lorentz <- unname(fitted_params ["sigma"])
     alpha_Lorentz <- unname(fitted_params ["alpha_Lorentz"])
     expl_var <- (sum((Fourier_Amp_model)^2) / sum((PP$Amp^2)))
     Lorentz_model = list(Frequency = PP$freq, Fourier_Amps = PP$Amp, modelled_Fourier_Amps = Fourier_Amp_model)
     warn = result_fitting$warnings

    res <- list(
      freq_Lorentz = freq_Lorentz,
      scale_Lorentz = scale_Lorentz,
      alpha_Lorentz = alpha_Lorentz,
      expl_var = expl_var,
      Lorentz_model = Lorentz_model,
      WARN = warn
    )
    }


  return(res)
}


# Auxiliary Function

# author Luke Tierney (2004), R-help post
#	https://stat.ethz.ch/pipermail/r-help/2004-June/052132.html

withWarnings <- function(expr) {
  W <- NULL
  wHandler <- function(w) {
    W <<- c(W, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = W)
}


