#' Frequency Estimation Using the Adapted Variable Period Technique
#'
#' @description This function computes the frequency of a time series based on
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

freq_adapted_variable_period <- function(x, delta_t = 1.0) {
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
    is.finite(delta_t),
    delta_t > 0
  )


  ## ---------------------------
  ## Handle constant signal
  ## ---------------------------

  var_x <- stats::var(x)

  if (var_x == 0) {
    warning("'x' has zero variance")
    return(
      list(
        freq_AVP = NA_real_,
        Max_AVP = NA_real_,
        ts_length_considered = NA_real_,
        expl_var = NA_real_
      )
    )
  }



  ## ---------------------------
  ## Setup
  ## ---------------------------
  N <- length(x)
  NP_seq <- seq.int(from = floor(N / 2), to = N)

  ## ---------------------------
  ## Evaluate all candidate lengths
  ## ---------------------------
  res_mat <- t(vapply(
    NP_seq,
    Periodogram_sampled_fast,
    numeric(2),
    x = x,
    delta_t = delta_t
  ))

  colnames(res_mat) <- c("freq", "power")

  ## ---------------------------
  ## Select optimum
  ## ---------------------------
  best_idx <- which.max(res_mat[, "power"])

  freq_AVP <- unname(res_mat[best_idx, "freq"])
  Max_AVP <- unname(res_mat[best_idx, "power"])
  NP_best <- NP_seq[best_idx][1]

  expl_var <- unname(Max_AVP / var_x / (N - 1))

  ## ---------------------------
  ## Output
  ## ---------------------------
  list(
    freq_AVP = freq_AVP,
    Max_AVP = Max_AVP,
    ts_length_considered = NP_best,
    expl_var = expl_var
  )
}

#
# AUXILIARY FUNCTIONS
#

ifft <- function(x) {
  Conj(stats::fft(x, inverse = TRUE)) / length(x)
}


Periodogram_sampled_fast <- function(x, assumed_ts_length, delta_t) {
  N <- length(x)

  ## ---------------------------
  ## Zero padding (more efficient)
  ## ---------------------------
  N_pad <- ceiling(N / assumed_ts_length) * assumed_ts_length
  x_pad <- c(x, rep(0, N_pad - N))

  dim(x_pad) <- c(assumed_ts_length, N_pad / assumed_ts_length)

  ## Indicator (instead of recomputing every time)
  ind <- c(rep(1, N), rep(0, N_pad - N))
  dim(ind) <- dim(x_pad)

  ## ---------------------------
  ## Aggregation
  ## ---------------------------
  xp <- rowSums(x_pad)
  xn <- rowSums(ind)

  ## ---------------------------
  ## FFTs
  ## ---------------------------
  XP <- stats::fft(xp)
  XN <- stats::fft(xn)

  PP <- Mod(XP)^2 / assumed_ts_length^2
  PN <- Mod(XN)^2 / assumed_ts_length^2

  ## ---------------------------
  ## Stable division (avoid NaN)
  ## ---------------------------
  denom <- Re(ifft(PN))
  denom[denom == 0] <- .Machine$double.eps

  C <- Re(ifft(PP)) / denom

  ## ---------------------------
  ## Final periodogram
  ## ---------------------------
  P <- Re(stats::fft(C))

  half <- floor(assumed_ts_length / 2)
  periodogram <- 2 * P[2:(half + 1)]

  freqs <- (seq_len(half) / assumed_ts_length) / delta_t

  idx <- which.max(periodogram)

  c(freq = freqs[idx], power = periodogram[idx])
}
