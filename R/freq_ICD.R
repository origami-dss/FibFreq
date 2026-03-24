#' Frequency Estimation Using the Interval Length between Succeeding Maxima as Identified by an Algorithm Implemented in ICDs
#' @description This function implements the sensing algorithm used by defibrillators of the Biotronic company for estimating the
#' heart rate and the fibrillation frequency. The parameters are optimized for the data set included in this library, i.e., for mice ECGs.
#'
#' @param x             a real valued array containing the values of the time series
#' @param thresh_min    a positive real value, the length of the minimum threshold for peak detection
#' @param amp_fac       a positive real value, the factor defining the upper threshold
#' @param delta_t_u     a positive real value, the length of the upper threshold time interval (in time units), default value is delta_t_u = 0.026
#' @param delta_t_l     a positive real value, the length of the lower threshold time interval (in time units), default value is delta_t_l = 0.026
#' @param delta_t       a positive real value, the sampling time of the algorithm (in time units), default value is delta_t = 0.001
#' @param ...           optional arguments to be passed to the function splus2R::peaks. Otherwise the values span = 11, strict = FALSE and endbehavior = 0 are used.
#'
#'
#' @return  a named list containing the following components:
#' \itemize{
#'   \item `freq_ICD` - the estimated frequency.
#'   \item `c_v` - the coefficient of variation of the time intervals between  the identified peaks.
#'   \item `no_of_peaks` - the number of peaks.
#'   \item `maxima_sampled` -  the time series indices of the identified peaks.
#' }
#' @export
#' @references{ Brueggemann T, Dahlke D, Chebbo A, and Neumann I: *Tachycardia detection in modern implantable cardioverter–defibrillars*. Herzschrittmachertherapie and Elektrophysiologie 27, 171-185 (2016) }
#' @references{ Diaz-Maue L, Witt A, Nobach H: *Unraveling Cardiac Arrhythmia Frequency, Comparative Analysis Using Time and Frequency Domain Algorithms*. Front. Signal Process., Sec. Biomedical Signal Processing 5 (2025),  \url{https://doi.org/10.3389/frsip.2025.1707422}}‚
#' @examples
#' # Let's Consider two synthetic time series and an example from the attached data set:
#' x1 = sin(0.02 * 2*pi * (1:1000))
#' x2 = sin(0.02 *  2*pi * (1:1000)) + 1.1 * sin( 0.025 * 2*pi * (1:1000))
#' ecg_6  <- MiceFibECGs[,6]
#'
#' freq_ICD(x1)
#' freq_ICD(x2)
#' # The irregularity of the fibrillation is indicated by a high value of the coefficient of variation
#' freq_ICD(ecg_6)




freq_ICD <- function(x,
                     thresh_min = 0.02,
                     amp_fac = 0.4,
                     delta_t_u = 0.026 ,
                     delta_t_l = 0.026,
                     delta_t = 0.001,
                     ...)
{
  validate_numeric <- function(x, len = NULL, finite = TRUE, positive = FALSE, arg_name = "x") {
    if (is.null(x)) stop(sprintf("'%s' must not be NULL", arg_name))
    if (!is.numeric(x) || !is.vector(x)) stop(sprintf("'%s' must be a numeric vector", arg_name))
    if (!is.null(len) && length(x) != len) stop(sprintf("'%s' must be of length %d", arg_name, len))
    if (any(is.na(x))) stop(sprintf("'%s' contains NA values", arg_name))
    if (finite && any(!is.finite(x))) stop(sprintf("'%s' must be finite", arg_name))
    if (positive && any(x <= 0)) stop(sprintf("'%s' must be positive", arg_name))
  }

  # Validations
  validate_numeric(x, finite = TRUE, arg_name = "x")
  if (length(x) < 3L) stop("data series too short")

  validate_numeric(thresh_min, len = 1, finite = TRUE, arg_name = "thresh_min")
  validate_numeric(amp_fac,    len = 1, finite = TRUE, positive = TRUE, arg_name = "amp_fac")
  validate_numeric(delta_t_u,  len = 1, finite = TRUE, positive = TRUE, arg_name = "delta_t_u")
  validate_numeric(delta_t_l,  len = 1, finite = TRUE, positive = TRUE, arg_name = "delta_t_l")
  validate_numeric(delta_t,    len = 1, finite = TRUE, positive = TRUE, arg_name = "delta_t")

  if (delta_t_l < delta_t) stop("length of lower threshold time interval smaller than sampling interval")
  if (delta_t_u < delta_t) stop("length of upper threshold time interval smaller than sampling interval")

  res <- list(
    freq_ICD = NA_real_,
    c_v = NA_real_,
    no_of_peaks = 0,
    maxima_sampled = integer()
  )

  if (stats::var(x) == 0) {
    message("'x' does not contain fluctuations, returning NULL")
  } else if (all(x < thresh_min)) {
    message("'x' does not contain values above 'thresh_min'")
  } else if (all(x >= thresh_min)) {
    message("'x' does not contain values below 'thresh_min'")
  } else if (!any(upward_threshold_crossing(x, thresh_min))) {
    message("'x' no threshold crossings")
  } else
  {
    args <- list(...)

    allowed_args <- c("span", "strict", "endbehavior")
    if (length(args) > 0 && !all(names(args) %in% allowed_args)) {
      message("\"...\" contains unknown and unused arguments")
    }

    # Default arguments
    args_max <- list(span = 11, strict = FALSE, endbehavior = 0, x = x)

    # Update defaults with provided args (only allowed ones)
    args_max <- modifyList(args_max, args[intersect(names(args), allowed_args)])

    # Ensure x is always set correctly
    args_max$x <- x


    local_max <- do.call(splus2R::peaks, args_max)
    local_max_over_thresh_min <- (local_max & (x > thresh_min))

    if (length(which(local_max_over_thresh_min)) < 3)
    {
      res <-  list(
        freq_ICD = NA_real_,
        c_v = NA_real_,
        no_of_peaks = 0,
        maxima_sampled = integer()
      )
      message("'x' does not contain enough local maxima  for frequency estimation")
    } else
    {
      l <- length(x)

      ##  deltas in sampling interval (delta_t) units
      n_u <- floor(delta_t_u / delta_t)
      n_l <- floor(delta_t_l / delta_t)

      # helper signal for downward threshold crossing
      up_down <- sign(diff(x))
      utc <-  upward_threshold_crossing(x, thresh_min)
      pos_utc = which(utc)

      # initialize variables for searching
      Maxima <- NULL
      Timings <- NULL
      index_of_peak <- 1
      pos_upward_crossing <- pos_utc[1]


      ## Identify first peak.    # length(ww) > 0 (was tested above)
      ww <- which(local_max_over_thresh_min)
      ww <- ww[ww > min(pos_upward_crossing)]
      Timings[1] <- ww[1]        # timing of the first peak that is larger than thresh_min
      Maxima[1] <- x[Timings[1]] # amplitude of the first peak (either first R-peak or first max. of the fibrillation) that is larger than thresh_min
      index_of_peak = 1


      searching_interval_begin <- max(pos_utc[pos_utc < Timings[1]]) + 1
      searching_interval_end <-  searching_interval_begin + n_u
      thresh <- max(c(Maxima[1] * amp_fac, thresh_min))

      repeat {
        # (A) Searching for downward threshold  crossing (indication of 'end' of the peak, the time interval has the length delta_t_u, the threshold is amp_fac times  the peak height)

        n <- n_u
        repeat {
          DC <-  which(x[searching_interval_begin:searching_interval_end] <= thresh &
                         up_down[searching_interval_begin:searching_interval_end] == -1) ### oder ???? == -1
          if (length(DC) > 0) {
            searching_interval_begin <- searching_interval_begin  + DC[1] - 1
            searching_interval_end <- searching_interval_begin  + n_l
            break
          } else {
            n <- n + 1
            searching_interval_end <- searching_interval_end + 1
          }

          ##  print(c("hoi",  searching_interval_begin, searching_interval_end, searching_interval_end - searching_interval_begin +1 ))
          if (searching_interval_end > l)
            break

        }
        if (searching_interval_end > l)
          break

        ###  (B) Searching for upward Threshold Crossing (pos_upward crossing) indicating the next peak,
        ### Loop over lowering and rightwards shifting thresholds

        no_of_time_interval = 0

        repeat {
          no_of_time_interval <- no_of_time_interval + 1
          if (no_of_time_interval == 1)
            thresh <- max(c(Maxima[index_of_peak] * 0.25, thresh_min))
          else
            thresh <- max(c(thresh * 0.875, thresh_min))

          w <- which(upward_threshold_crossing(x[searching_interval_begin:searching_interval_end], thresh))

          if (length(w) > 0) {
            pos_upward_crossing <- searching_interval_begin - 2 + w[1]
            break
          }  else
          {
            searching_interval_begin <- searching_interval_end - 1
            searching_interval_end <- searching_interval_begin + n_l
            if (length(w) > 0 | searching_interval_end > l)
              break
          }
        }

        if (searching_interval_end > l)
          break

        ### (C) Identify the succeeding local maximum (either R-wave or max. of fibrillation) called position_max

        n <- n_l
        repeat {
          preceeding_upward_crossing_of_min_thresh <- max(pos_utc[pos_utc < pos_upward_crossing])
          if (preceeding_upward_crossing_of_min_thresh  > Timings[index_of_peak])
            searching_interval_begin <-  preceeding_upward_crossing_of_min_thresh
          else
            searching_interval_begin <- pos_upward_crossing

          searching_interval_end <- pos_upward_crossing + n
          time_index_local_max <-  which(local_max[searching_interval_begin:searching_interval_end])
          position_max <- 0
          if (length(time_index_local_max) > 0)  {
            if (length(time_index_local_max) > 1)
              w = which.max(x[searching_interval_begin + time_index_local_max - 1])
            else
              w <- 1

            position_max <- searching_interval_begin  + time_index_local_max[w] -
              1
            index_of_peak <- index_of_peak + 1
            Timings[index_of_peak] <- position_max       # timing of the current peak
            Maxima[index_of_peak] <- x[Timings[index_of_peak]]  # amplitude of the current peak

          } else
          {
            n <- n + 1
          }
          if (position_max > 0 | searching_interval_end > l)
            break
        }


        # end of find local max
        searching_interval_begin <- Timings[index_of_peak]
        searching_interval_end <-  pos_upward_crossing + n
        thresh <-  max(c(x[Timings[index_of_peak]] * amp_fac, thresh_min))

        if (searching_interval_end > l)
          break
      }


      if (index_of_peak > 1) {
        freq_ICD = (index_of_peak - 1) / (delta_t * diff(range(Timings)))
        c_v = coefficient_of_variation(diff(Timings*delta_t))

        res <- list(
          freq_ICD = freq_ICD,
          c_v = c_v,
          no_of_peaks = index_of_peak,
          maxima_sampled = Timings
        )
      } else
        res <- list(
          freq_ICD = NA_real_,
          c_v = NA_real_,
          no_of_peaks  = index_of_peak,
          maxima_sampled = Timings
        )

    }

  }
  return(res)
}

#
# AUXILIARY FUNCTION
#

upward_threshold_crossing <- function(x, thresh)
{
    x_over_thresh <-  sign(x - thresh)
    names(x_over_thresh) <- 1:length(x_over_thresh)
    x_over_thresh <- x_over_thresh[x_over_thresh != 0]


    w <- which(diff(x_over_thresh) == 2)


    thresh_upward_crossings <- rep(FALSE, length(x))
    thresh_upward_crossings[as.integer(names(w))] <- TRUE

    return(thresh_upward_crossings)
  }
