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

freq_threshold_crossing <- function(x, delta_t = 1.0, thresh = 0)
{
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(!is.finite(x)))
    stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) < 2)
    stop("data series to short")
  if (length(delta_t) != 1L |
      !is.numeric(delta_t) |
      !is.finite(delta_t) |
      is.na(delta_t) |
      delta_t <= 0)
    stop("'delta_t' must be positive finite numeric of length one")
  if (length(thresh) != 1L |
      !is.numeric(thresh) |
      is.na(thresh) |
      !is.finite(thresh))
    stop("'thresh' must be finite numeric of length one")
  if (stats::var(x) == 0)
  {
    message("'x' does not contain fluctuations, returning NA")

    res = list(
      freq_threshold_crossing = NA_real_,
      c_v = NA_real_,
      n_thresh = NA_real_,
      threshold_crossings = integer()
    )
  } else {
    if (missing(thresh))
      thresh = mean(x)

    Timings <- which(upward_threshold_crossing(x, thresh))
    if (length(Timings) < 3) {
      message("'x' does not contain enough threshold crossings, returning NA")

      res <- list(
        freq_threshold_crossing = NA_real_,
        c_v = NA_real_,
        n_thresh = NA_real_,
        threshold_crossings = integer(0)
      )
    } else
    {
      ISI_ZC <-  diff(Timings) * delta_t
      n_ISI_ZC <- length(ISI_ZC)

      freq_TC <-  1. / mean(ISI_ZC)
      c_v <- coefficient_of_variation(Timings)

      res <- list(
        freq_threshold_crossing = freq_TC,
        c_v = c_v,
        n_thresh = length(Timings),
        threshold_crossings = Timings
      )

    }
  }
  return(res)
}
