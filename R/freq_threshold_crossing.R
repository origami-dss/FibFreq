#' Frequency Estimation Using the Interval Length between Succeeding Upward Threshold Crossings
#'
#' @description This frequency estimation function computes the fundamental frequency as the inverse of the  average distance
#' between successive upward threshold crossings. It requires smooth and oscillating time series.
#'
#' @param x x a non-infinite real-valued numeric vector containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param thresh a real value, the considered threshold,  default value is thresh = 0.0
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_threshold_crossing` - the estimated frequency.
#'   \item `c_v` - the coefficient of variation of the time intervals between succeeding upward threshold crossings.
#'   \item `n_thresh` - the number of upward threshold crossings.
#'   \item `threshold_crossings` -  the time series indices of upward threshold crossings.
#' }
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. submitted to Frontiers in Signal Processing (2025)}
#' @examples
#' # Let's consider a synthetic time series and an example from the attached data set:
#' x1 <- sin( 0.05 * 2*pi * (1:100))
#' ecg_6  <- MiceFibECGs[,6]
#'
#' freq_threshold_crossing(x1)
#'
#' # Changing the threshold can change the resulting frequency
#' freq_threshold_crossing(x1, thresh = 0.5)
#'
#'# The irregularity of the fibrillation is indicated by a high value of the coefficient of variation
#' freq_threshold_crossing(ecg_6, delta_t = 0.001)
#'

freq_threshold_crossing <- function(x, delta_t = 1.0, thresh = 0) {

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


  if (!is.null(thresh)) {
    if (!is.numeric(thresh) || length(thresh) != 1L || !is.finite(thresh)) {
      stop("'thresh' must be a single finite number")
    }
  } else {
    thresh <- mean(x)
  }

  # ---- Helper for NA result ----
  empty_result <- function() {
    list(
      freq_threshold_crossing = NA_real_,
      c_v = NA_real_,
      n_thresh = NA_integer_,
      threshold_crossings = integer(0)
    )
  }

  # ---- No variation case ----
  if (stats::var(x) == 0) {
    message("'x' does not contain fluctuations, returning NA")
    return(empty_result())
  }

  # ---- Compute threshold crossings ----
  timings <- which(upward_threshold_crossing(x, thresh))

  if (length(timings) < 3L) {
    message("'x' does not contain enough threshold crossings, returning NA")
    return(empty_result())
  }

  # ---- Compute metrics ----
  isi <- diff(timings) * delta_t

  result <- list(
    freq_threshold_crossing = 1 / mean(isi),
    c_v = coefficient_of_variation(isi),
    n_thresh = length(timings),
    threshold_crossings = timings
  )

  return(result)
}
