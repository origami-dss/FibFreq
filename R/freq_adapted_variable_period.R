#' Frequency Estimation Using the Adapted Variable Period Technique
#'
#' @description This function computes the freqeuncy of a time series based on
#' the adapted variable period technique (see: ...).
#' In contrast to the direct evaluation of the power spectrum,
#' this method is considering cycle lengths which correspond to an
#' integer multiple of the sampling frequency.
#'
#' @param x real valued array containing the values of the time series
#' @param delta_t a real value, the sampling time, default value is delta_t = 1.0
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_AVP` - the best-fitting frequency estimated with the Adapted Variable Period Technique.
#'   \item `Max_P` - the corresponding maximum value of the adapted periodogram.
#'   \item `ts_length_considered` - the time series length used for the computation.
#'   \item `expl_var` -  The explained variance of the frequency component `freq_AVP.
#' }
#'
#' @return a named list containing
#'        (i) the best-fitting frequency estimated with the Adapted Variable Period Technique,
#'        (ii) the corresponding maximum value of the adapted periodogram,
#'        (iii) the time series length used for the computation and
#'        (iv) the explained variance.
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. submitted to Frontiers in Signal Processing (2025)}
#' @examples
#' # Let's consider three synthetic time series and an example from the attached data set:
#' x1 <- sin( 0.05 * 2 * pi * (1:100))
#' x2 <- sin( 0.05 * 2 * pi * (1:100)) + 1.1 * sin(0.07 * 2 * pi * (1:100))
#' x3 <- sin( 0.05 * 2 * pi * (1:100)) + sin(0.07 * 2 * pi * (1:100))
#' ecg_6  <- MiceFibECGs[,6]

#' freq_adapted_variable_period (x1)
#' freq_adapted_variable_period(x2)
#' freq_adapted_variable_period (x3)
#'
#' freq_adapted_variable_period(ecg_6, delta_t = 0.001)
#' # which results in a different frequency and in particular
#' # in a higher explained variance than the standard spectral method
#' freq_argmax_periodogram(ecg_6, delta_t = 0.001)
#'

freq_adapted_variable_period <- function(x,
                              delta_t = 1.0) {
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(!is.finite(x)))
    stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L)
    stop("data series to short")
  if (length(delta_t) != 1L |
      !is.numeric(delta_t) |
      !is.finite(delta_t) |
      delta_t <= 0)
    stop("'delta_t' must be positive finite numeric of length one")
  if (stats::var(x) == 0)
  {
    res <- c(freq_optim = NA_real_, Max_AVP = NA_real_, ts_length_considered = NA_real_, expl_var = NA_real_)
    message("time series 'x' does not contain fluctuations, returning NA")
  } else
  {
    N <- length(x)
    N_half <- floor(N/2)

    result = array(c(0), dim = c(2, 1 + ceiling(N / 2)))

    for (NP in floor(N / 2):N)
      result[, NP + 1 - floor(N / 2)] <- Periodogram_sampled(x, NP, delta_t)

    mp <- which.max(result[2, ])

    Max <- result[2, mp]
    freq_AVP <- result[1, mp]
    expl_variance <- Max/stats::var(x)/ (length(x)-1)
    NP <- floor(N / 2) -1 + mp

    res <- list(freq_AVP = freq_AVP, Max_AVP = Max, ts_length_considered = NP, expl_var = expl_variance)
  }
  return(res)
}


#
# AUXILIARY FUNCTIONS
#

ifft <- function(x) {
  Conj(stats::fft(x)) / (2 * pi)
}


Periodogram_sampled <- function(x, assumed_ts_length, delta_t)
{
  N <- length(x)
  N_rem <- N %% assumed_ts_length
  xxp <- array(c(x, rep(0, assumed_ts_length - N_rem)), dim = c(assumed_ts_length, N / assumed_ts_length + 1))
  xxn <- array(c(rep(1, N), rep(0, assumed_ts_length - N_rem)),
               dim = c(assumed_ts_length, N / assumed_ts_length + 1))

  xp <- rowSums(xxp)
  xn <- rowSums(xxn)
  ## Fourier Transform of xp and xn
  XP <- stats::fft(xp)
  XN <- stats::fft(xn)

  ## Periodograms of XP and XN
  PP <- abs(XP)^2 / assumed_ts_length^2
  PN <- abs(XN)^2 / assumed_ts_length^2

  ## Correlation function of XP
  C_coarse_grained <- Re(ifft(PP)) / Re(ifft(PN))

  ## Fourier transform of the correlation function -> Periodogram of xp with respect to zero padding

  P <- Re(stats::fft(C_coarse_grained))
  periodogram <- 2 * P[2:(floor(assumed_ts_length / 2) + 1)]
  freqs = seq(1/assumed_ts_length, 0.5, by = 1./assumed_ts_length) / delta_t
  idx <- which.max(periodogram)
  freq_argmax <- freqs[idx]
  max_periodogram <- periodogram[idx]

  res = c(freq_argmax = freq_argmax, max_periodogram = max_periodogram)

  return (res)
}

