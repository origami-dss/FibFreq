#' Title
#'
#' @param x             a real valued array containing the values of the time series
#' @param thresh_min    a positive real value, the length of the minimum threshold for peak detection
#' @param amp_fac       a positive real value, the factor defining the upper threshold
#' @param delta_t_u     a positive real value, the length of the upper threshold time interval (in time units), default value is delta_t_u = 26
#' @param delta_t_l     a positive real value, the length of the lower threshold time interval (in time units), default value is delta_t_l = 26
#' @param delta_t       a positive real value, the sampling time of the algorithm (in time units), default value is delta_t = 1
#'
#' @return              a vector with the estimated frequency, its standard deviation, its interquartile range and the number of identified maxima
#' @export
#'
#' @examples
#'




freq_ICD <- function(x, thresh_min = 0.02, amp_fac = 0.4,  delta_t_u = 26 , delta_t_l = 26, delta_t = 1 )
{


  if (!is.vector(x) | !is.numeric(x) | any(is.na(x)) | any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(thresh_min) | length(thresh_min) != 1L | any(is.infinite(thresh_min)) | any(thresh_min <= 0 )) stop("'thresh_min' must be a finite and positive  numeric of length one")
  if (!is.numeric(amp_fac) | length(amp_fac) != 1L | any(is.infinite(amp_fac)) | any(amp_fac <= 0) ) stop("'amp_fac' must be a finite and positive  numeric of length one")
  if (!is.numeric(delta_t_u) | length(delta_t_u) != 1L | any(is.infinite(delta_t_u)) | any(delta_t_u <= 0 )) stop("'delta_t_u' must be finite and positive numeric of length one")
  if (!is.numeric(delta_t_l) | length(delta_t_l) != 1L | any(is.infinite(delta_t_l)) | any(delta_t_l <= 0 )) stop("'delta_t_l' must be finite and positive numeric of length one")
  if (!is.numeric(delta_t) | length(delta_t) != 1L | any(is.infinite(delta_t)) | any(delta_t <= 0 )) stop("'delta_t' must be finite and positive numeric of length one")

  if (var(x) == 0)
  {
    res <- c(freq = NULL)
    message("'x' does not contain fluctuations, returning NULL")
  }
  if (all(x < thresh_min))
  {
    res <- c(freq = NULL)
    message("'x' does not contain values above 'thresh_min'")
  }

  if (all(x >= thresh_min))
  {
    res <- c(freq = NULL)
    message("'x' does not contain values below 'thresh_min'")
  }

  if (delta_t_u < delta_t)
  {
    res <- c(freq = NULL)
    message("'x' length of upper threshold time interval smaller than sampling interval")
  }
  if (delta_t_l < delta_t)
  {
    res <- c(freq = NULL)
    message("'x' length of lower threshold time interval smaller than sampling interval")
  }



  l <- length(x)

  ##  deltas in sampling interval (delta_t) units
  n_u <-floor(delta_t_u/delta_t)
  n_l <- floor(delta_t_l/delta_t)

  # identify the local maxima including treatment of flats
  u_d <- sign(diff(x))
  names(u_d) <- 1:length(u_d)
  u_d <- u_d[u_d != 0]
  local_max <- rep(FALSE, l)
  w= which(diff(u_d) == -2)
  if (length(w) == 0)
  {
    res <- c(timings = NULL, maxima = NULL)
    message("'x' does not contain local maxima")
  }
  local_max[as.integer(names(w))] <- TRUE

  # identify the positions of thresh_min upward crossings
  x_over_thresh_min <-  sign(x - thresh_min)
  names(x_over_thresh_min) <- 1:length(x_over_thresh_min)
  x_over_thresh_min <- x_over_thresh_min[x_over_thresh_min != 0]

  w <- which(diff(x_over_thresh_min) == 2)
  if (length(w) == 0)
  {
    res <- c(timings = NULL, maxima = NULL)
    message("'x' does not contain 'thresh_min' upward crossings")
  }

  no_of_thresh_min_upward_crossings <- rep(0, l)
  no_of_thresh_min_upward_crossings[as.integer(names(w))] <- 1
  no_of_thresh_min_upward_crossings = (diffinv(no_of_thresh_min_upward_crossings))[-1]
  pos_upward_crossing = as.integer(names(w[1]))

  # consider only local maxes after thresh_min crossing
  local_max[no_of_thresh_min_upward_crossings == 0] <- FALSE

 # define local maxima which are above threshold
  local_max_over_thresh_min <- local_max
  local_max_over_thresh_min[x <= thresh_min] <- FALSE
  if (all(!local_max_over_thresh_min))
  {
    res <- c(timings = NULL, maxima = NULL)
    message("'x' does not contain local maxima larger than thresh_min")
  }



#  up_down helper signal for downward threshold crossing
  up_down <- sign(diff(x))


  # initialize variables for searching
  MAXs <- NULL
  TIMINGs <- NULL
  PROLONGED_INTERVAL_THRESH_DOWN <- NULL
  PROLONGED_INTERVAL_THRESH_UP <- NULL
  no_of_peak <- 1

plot(x, type="l")
abline(h = thresh_min, col="grey")

  ## Identify first peak.
  ww = which(local_max_over_thresh_min)     # length(ww) > 0 (was tested above)
  TIMINGs[1] <- ww[1]        # timing of the first peak that is larger than thresh_min
  MAXs[1] <- x[TIMINGs[1]] # amplitude of the first peak (either first R-peak or first max. of the fibrillation) that is larger than thresh_min
  PROLONGED_INTERVAL_THRESH_DOWN[1] <- FALSE
  PROLONGED_INTERVAL_THRESH_UP[1] <- FALSE


points(TIMINGs, MAXs, pch = 22, bg = "darkred")



  searching_interval_begin <- TIMINGs[1]+1
  searching_interval_end <-  pos_upward_crossing + n_u
  thresh <- max(c(MAXs[1] * amp_fac, thresh_min))

  repeat
  {

# (A) Searching for downward threshold  crossing (indication of 'end' of the peak)

  n <- n_u
  repeat
  {

    DC <-  which(x[ searching_interval_begin :  searching_interval_end] <=  thresh & up_down[ searching_interval_begin :  searching_interval_end] < 1) ### oder ???? == -1

    if(length(DC) >0)
    {
lines(c(searching_interval_begin, searching_interval_end), c(thresh - 0.0, thresh - 0.00), col="darkviolet", lwd=2)
#      searching_interval_end <- searching_interval_begin -1
      searching_interval_begin <- TIMINGs[no_of_peak] + DC[1]

      break
    } else {
      n <- n + 1
      PROLONGED_INTERVAL_THRESH_DOWN[no_of_peak] <- TRUE
      searching_interval_end <- pos_upward_crossing + n
      if ( searching_interval_end > l) break
    }
  }

###  (B) Searching for upward Threshold Crossing (pos_upward crossing) indicating the next peak
### Loop over lowering and rightwards shifting thresholds

    no_of_time_interval = 0

    repeat
    {
      no_of_time_interval <- no_of_time_interval + 1
      if (no_of_time_interval == 1) thresh <- max(c(amp_fac * MAXs[no_of_peak], thresh_min))  else if (no_of_time_interval == 2) thresh <- max(c(MAXs[no_of_peak]*0.25, thresh_min)) else thresh <- max(c(thresh* 0.875, thresh_min))

      w <- which((x[searching_interval_begin : searching_interval_end] > thresh) & (up_down[searching_interval_begin : searching_interval_end] > -1))

lines(c(searching_interval_begin, searching_interval_end), c(thresh,thresh), lwd = no_of_time_interval)

      if (length(w) >0) pos_upward_crossing <- searching_interval_begin -2 + w[1]  else {
          searching_interval_begin <- searching_interval_end
        searching_interval_end <- searching_interval_begin + n_l
      }

points(c(pos_upward_crossing, pos_upward_crossing+1), c(x[pos_upward_crossing], x[pos_upward_crossing+1]), pch = 20, bg = "black", cex = 0.5)
lines(c(pos_upward_crossing, pos_upward_crossing+1), c(x[pos_upward_crossing], x[pos_upward_crossing+1]), lwd=3, col = "darkred")
      if (length(w)>0 | searching_interval_end >l )  break
    }

  if(searching_interval_end >l) break



### (C) Identify the succeeding local maximum (either R-wave or max. of fibrillation) called position_max
    n <- n_u
    repeat
    {
      time_index_local_max <-  which(local_max[pos_upward_crossing:(pos_upward_crossing + n)] & x[pos_upward_crossing:(pos_upward_crossing + n)] > thresh)

      if( length(time_index_local_max) >0)  {
        if( length(time_index_local_max) >1) w= which.max(x[ pos_upward_crossing + time_index_local_max - 1]) else w <-1

        position_max <- pos_upward_crossing + time_index_local_max[w] -1
        no_of_peak <- no_of_peak + 1
        TIMINGs[no_of_peak] <- position_max       # timing of the current peak
        MAXs[no_of_peak] <- x[TIMINGs[no_of_peak]]  # amplitude of the current peak
        PROLONGED_INTERVAL_THRESH_DOWN[no_of_peak] <- FALSE
        PROLONGED_INTERVAL_THRESH_UP[no_of_peak] <- FALSE

rect( pos_upward_crossing, MAXs[no_of_peak]*amp_fac, pos_upward_crossing + n_u, MAXs[no_of_peak],  border="blue" )
points(TIMINGs[no_of_peak], MAXs[no_of_peak], pch=22, bg="darkred")


      } else
        {
          PROLONGED_INTERVAL_THRESH_DOWN[no_of_peak] <- TRUE    ##### und jetzt? interval verbreitern?  oder nächstes max suchn, dass über dem threshold liegt???
          n <- n+1
        }
    if (length(position_max)>0 | searching_interval_end >l ) break
    }
    searching_interval_begin <- TIMINGs[no_of_peak] + 1
    searching_interval_end <-  pos_upward_crossing + n
    thresh <-  max(c(x[TIMINGs[no_of_peak]] * amp_fac, thresh_min))

    if (searching_interval_end >l ) break
    }



  return(cbind(timings = TIMINGs, maxima = MAXs, PROLONGED_INTERVAL_THRESH_UP, PROLONGED_INTERVAL_THRESH_DOWN))
}

