#' Frequency Estimation by Evaluating the Max-to-Max Time Intervals
#'
#' @description Frequency estimation based on the determination of the average peak-to-peak distance.
#' This function requires smooth and oscillating time series.
#' The peaks are identified with the splus2R::peaks function.
#'
#' @param x a non-infinite real-valued numeric vector containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param ...  optional arguments to be passed to the function splus2R::peaks. Otherwise span = 11, strict = TRUE and endbehavior = 0 are used.
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
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. submitted to Frontiers in Signal Processing (2025)}
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

freq_max2max <- function(x, delta_t = 1.0, ...)
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
      any(!is.finite(delta_t)) |
      delta_t <= 0)
    stop("'delta_t' must be positive (>0) finite numeric of length one")
  if (stats::var(x) == 0)
  {
    res <- list(
      freq_max2max = NA_real_,
      c_v = NA_real_,
      n_max = NA_real_,
      maxima_sampled = integer(0)
    )
    message("'x' does not contain fluctuations, returning NA")
  } else
  {
    args <- list(...)
    if (length(args) > 0 &
        !all(names(args) %in% c("span", "strict", "emdbehavior")))
      message("\"...\" contains unknown and unused arguments")


    # transfer the arguments for splus2R::peaks function
    args_max <- list(
      span = 11,
      strict = TRUE,
      endbehavior = 0,
      x = x
    )
    if ("span" %in% names(args))
      args_max$span <- args$span
    if ("strict" %in% names(args))
      args_max$strict <- FALSE
    if ("endbehavior" %in% names(args))
      args_max$endbehavior <-  args$endbehavior

    Timings <- do.call(splus2R::peaks, args_max)

    n_max <- length(which(Timings))
    if (n_max < 3)
    {
      res <- list(
        freq_max2max = NA_real_,
        c_v = NA_real_,
        n_max = n_max,
        maxima_sampled = which(Timings)
      )
      message("at least three identified maxima are required, returning NA")
    }
    else
    {
      ISI_Timings <-  diff(which(Timings)) * delta_t
      n_ISI_Timings <- length(ISI_Timings)

      freq_max2max <-  1. / mean(ISI_Timings)

      c_v = coefficient_of_variation(which(Timings))

      res <- list(
        freq_max2max = freq_max2max,
        c_v = c_v,
        n_max = n_max,
        maxima_sampled = which(Timings)
      )

    }
  }
  return(res)
}

coefficient_of_variation <- function(Timings)
{
  if (!is.vector(Timings) |
      !is.numeric(Timings) |
      any(is.na(Timings)) |
      any(!is.finite(Timings)) |
      length(Timings) < 3) {
    message (
      "vector of maxima must be a non-infinite real-valued numeric vector with at least two elements, returning NA"
    )

    cv = NA_real_
  } else
  {
    IEOT = diff(Timings)
    cv = stats::sd(IEOT) / mean(IEOT)
  }
  return(cv)
}
