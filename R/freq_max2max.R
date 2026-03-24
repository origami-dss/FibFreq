#' Frequency Estimation by Evaluating the Max-to-Max Time Intervals
#'
#' @description Frequency estimation based on the determination of the average peak-to-peak distance.
#' This function requires smooth and oscillating time series.
#' The peaks are identified with the splus2R::peaks function.
#'
#' @param x a non-infinite real-valued numeric vector containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param ...  optional arguments to be passed to the function splus2R::peaks. Otherwise span = 11, strict = TRUE and endbehavior = 0 are used.
#' @param .warn Logical; if TRUE (default), messages are shown for edge cases.
#'
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_max2max` - the estimated frequency.
#'   \item `c_v` - the coefficient of variation of the inter-maxima-intervals.
#'   \item `n_max` - the number of identified local maxima.
#'   \item `maxima_sampled` -   the time series indices of the identified local maxima.
#' }
#'
#' @importFrom splus2R peaks
#' @export
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. Front. Signal Process., Sec. Biomed. Signal Process. 5 (2025),  \url{https://doi.org/10.3389/frsip.2025.1707422}}
#' @examples
#' # Let's consider two synthetic time series and an example of the attached data set
#' x1 <- sin( 0.05 * 2 * pi * (1:100))
#' x2 <- sin(0.02 *  2*pi * (1:100)) + 1.1 * sin( 0.025 * 2*pi * (1:100))
#'
#' ecg_6  <- MiceFibECGs[,6]
#'
#' freq_max2max(x1)
#' freq_max2max(x2)
#'
#' freq_max2max(ecg_6, delta_t = 0.001)
#' # Changing optional parameters of splus2R::peaks can change the result
#' freq_max2max(ecg_6, delta_t = 0.001, span = 35)

freq_max2max <- function(x, delta_t = 1, ..., .warn = TRUE)
{
  ## ---------------------------
  ## Helpers
  ## ---------------------------
  empty_result <- function(n_max = 0L, maxima = integer()) {
    list(
      freq_max2max = NA_real_,
      c_v = NA_real_,
      n_max = n_max,
      maxima_sampled = maxima
    )
  }

  .msg <- function(txt) if (.warn) message(txt)

  ## ---------------------------
  ## Input validation
  ## ---------------------------
  if (!is.numeric(x) || length(x) <= 1L || anyNA(x) || any(!is.finite(x))) {
    stop("'x' must be a finite numeric vector of length > 1")
  }

  if (!is.numeric(delta_t) || length(delta_t) != 1L ||
      !is.finite(delta_t) || delta_t <= 0) {
    stop("'delta_t' must be a positive finite scalar")
  }

  var_x <- stats::var(x)

  ## ---------------------------
  ## No variability
  ## ---------------------------
  if (var_x == 0) {
    .msg("'x' has no variability, returning NA")
    return(empty_result())
  }

  ## ---------------------------
  ## Optional args handling
  ## ---------------------------
  dots <- list(...)
  valid_args <- c("span", "strict", "endbehavior")

  unknown <- setdiff(names(dots), valid_args)
  if (length(unknown) && .warn) {
    message("Ignoring unknown arguments: ", paste(unknown, collapse = ", "))
  }

  args_max <- modifyList(
    list(x = x, span = 11, strict = TRUE, endbehavior = 0),
    dots[intersect(names(dots), valid_args)]
  )

  ## ---------------------------
  ## Peak detection
  ## ---------------------------
  maxima_idx <- which(do.call(splus2R::peaks, args_max))
  n_max <- length(maxima_idx)

  if (n_max < 3L) {
    .msg("At least 3 maxima required, returning NA")
    return(empty_result(n_max, maxima_idx))
  }

  ## ---------------------------
  ## Metrics
  ## ---------------------------
  isi <- diff(maxima_idx) * delta_t

  list(
    freq_max2max = 1 / mean(isi),
    c_v = coefficient_of_variation(isi),
    n_max = n_max,
    maxima_sampled = maxima_idx
  )
}


coefficient_of_variation <- function(isi)
{
  if (!is.numeric(isi) || length(isi) < 2L ||
      anyNA(isi) || any(!is.finite(isi))) {
    return(NA_real_)
  }

  m <- mean(isi)
  if (m == 0) return(NA_real_)

  stats::sd(isi) / m
}
