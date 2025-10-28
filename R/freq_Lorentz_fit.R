#' Frequency Estimation Using a Lorentz Fit to the Amplitude Spectrum
#'
#' @description
#' This function is fitting a Lorentz function to the amplitudes of the Fourier transform (not to the Power spectrum!) of a time series.
#'
#' @param  x real valued array containing the values of the time series
#' @param delta_t  a positive real value, the sampling time, default value is delta_t = 1.0
#' @param f_min  a positive real value, the lower bound of the considered frequency interval
#' @param f_max  a positive real value, the upper bound of the considered frequency interval
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#' @param control 	an optional list of control settings for the nonlinear least-squares algorithm (i.e., the function nls).
#' See \link[stats]{nls.control} for the names of the settable control values and their effect.
#' @param algorithm  character string specifying the algorithm to use. The default is "plinear".  See   \link[stats]{nls} for other options.
#'
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_Lorentz` - the  center of the fitted Lorentzian.
#'   \item `scale_Lorentz` - the scale parameter of the fitted Lorentzian.
#'   \item `max_Lorentz_fit` - the  maximum value of the fitted Lorentzian.
#'   \item `expl_var` -  the  explained variance of the fitted Lorentzian.
#'   \item `Lorentz_model` -  a named list with the amplitude spectrum and the fitted model.
#' }
#'
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. submitted to Frontiers in Signal Processing (2025)}
#' @examples
#' # Let's consider two synthetic time series and an example from the attached data set
#' x1 = sin(0.11 * 2*pi *(1:100))
#' x2 = sin(0.11 * 2*pi *(1:100)) + 1.01 * sin(0.17 * 2*pi * (1:100))
#' ecg_6  <- MiceFibECGs[,6]
#'
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
#' plot(res$Lorentz_model$freq, res$Lorentz_model$FourierAmp, xlab = xlab, ylab = ylab, pch = 3)
#' lines(res$Lorentz_model$freq, res$Lorentz_model$LorentzFit, col = "red")
#'
#' legend("topright" , c("Fourier Amplitudes", "Lorentz Fit"),
#' col = c("black","red"), lty = c(0,1), pch = c(3,NA))
#'

freq_Lorentz_fit <- function(x,
                             delta_t = 1.0,
                             welch_window = FALSE,
                             f_min = 0,
                             f_max = Inf,
                             control = stats::nls.control(scaleOffset = 1),
                             algorithm = "plinear")
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
  if (f_max < f_min )
    stop("'f_max' < 'f_min'")
  if (f_min >  1. / 2 / delta_t)
    stop("'f_min' is larger than the maximum of the sampled frequencies")
  if (f_max <  1 / length(x) / delta_t)
    stop("'f_max' is smaller than the minimum of the sampled frequencies")
  if (stats::var(x) == 0)
  {
    warning("'x' does not contain fluctuations, returning NA")

    res <- list(freq_Lorentz = NA_real_, scale_Lorentz = NA_real_,  Max_Lorentz_Fit = NA_real_, expl_var = NA_real_, Lorentz_model = NULL)
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
    A <- A[2:(1 + N_half)]
    freq <- delta_f * (1:N_half)


    w <- which (freq >= f_min & freq <= f_max)
    if (length(w) < 4) stop("Frequency range contains less than 4 frequencies")
    if (length(w) < 10) warning("The Lorentzian is fitted to < 10 data points")
    freq <- freq[w]
    Amp <- A[w]
    PP <- data.frame(freq = freq, Amp = Amp)

    ww <- which.max(Amp)
    argmax_freq <-  freq[ww[1]]
    sigma_squared <- sum(Amp * (freq - argmax_freq)^2 / sum(Amp))
    sigma_start <- sqrt(sigma_squared)

  options(show.error.messages = FALSE)

   result_fitting <- try({stats::nls(
      Amp ~ sigma / ((freq - Lorentz_freq)^2 + sigma^2),
      data = PP,
      start = list(Lorentz_freq = argmax_freq, sigma = sigma_start),
      control = control,
      algorithm = algorithm)},
      silent = TRUE)

    options(show.error.messages = TRUE)
    if (inherits(result_fitting,"try-error")) {
      msg = geterrmessage()
      stop( paste0("Are 'f_min', 'f_max', 'delta_t' set correctly? Otherwise check the parameters of the 'nls' algorithm! \n ", msg))
      }


    modelled_Fourier_amplitude <- stats::predict(result_fitting)

    freq_Lorentz <-  result_fitting$m$getPars() ["Lorentz_freq"]
    scale_Lorentz <-result_fitting$m$getPars() ["sigma"]
    expl_var <- (sum((modelled_Fourier_amplitude)^2) / sum((PP$Amp)^2))
    Max_Lorentz_Fit <- max(modelled_Fourier_amplitude)

    Lorentz_model <- list(
      freq = PP$freq,
      FourierAmp = PP$Amp,
      LorentzFit = modelled_Fourier_amplitude
    )

    names(scale_Lorentz) <- NULL
    names(freq_Lorentz) <- NULL

    res <- list(
      freq_Lorentz = freq_Lorentz,
      scale_Lorentz = scale_Lorentz,
      Max_Lorentz_Fit = Max_Lorentz_Fit,
      expl_var = expl_var,
      Lorentz_model = Lorentz_model
    )

  }


  return(res)
}

