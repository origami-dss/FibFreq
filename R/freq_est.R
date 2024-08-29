
ifft <- function(x) {
  Conj(fft(x)) / (2*pi)
}



#' Frequency Estimation Using the Optimized Period Technique
#'
#' @param x real valued array containing the values of the time series
#' @param delta_t a real value, the sampling time, default value is delta_t = 1.0
#' @param f_min a real value, the minimum frequency of the tested frequency range
#' @param f_max a real value, the maximum frequency of the tested frequency range
#'
#' @return the best-fitting frequency estimated with the XXX algorithm (see ***)
#' @export
#'
#' @examples
#' x1 = sin(0.11*2*pi*(1:100));
#' x2 = sin(0.11*2*pi*(1:100))+ 1.01*sin(0.17*2*pi*(1:100));
#' freq_est(x1)
#' freq_est(x2)
#'
freq_est <- function(x, delta_t = 1.0, f_min , f_max , names = TRUE) {

  if(!is.vector(x) & !is.numeric(x) & any(is.na(x)) & any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) & length(delta_t) != 1L & any(is.infinite(delta_t))) stop("'delta_t' must be finite numeric of length one")
  if(delta_t <= 0) stop("'delta_t' must be positive (>0)")
  if (!is.numeric(f_min) & length(f_min) != 1L & any(is.infinite(delta_t))) stop("'f_min' must be finite numeric of length one")
  if (!is.numeric(f_max) & length(f_max) != 1L & any(is.infinite(delta_t))) stop("'f_max' must be finite numeric of length one")
  if (f_min >= f_max) stop("'f_min < f_max' must be true")
  if (f_min < 1/length(x)/delta_t) stop("'f_min' must be inside frequency domain")
  if (f_max > 0.5/delta_t) stop("'f_max' must be inside the frequency domain")


#  HOLGER'S VERSION:
#  ftm=0
  ftm <- f_min
  ampm <- 0.0
  N <- length(x)
  xx <- x - mean(x)   # demeaned version of the time series x

#  HOLGER'S VERSION:
#  for (NP in 2:N) {


  for (NP in max(c(2,floor(1/(delta_t*f_max)))) : min(c(floor(c(1/(delta_t*f_min),N))))) {
#  HOLGER'S VERSION:
#   xp = rep(0, NP)
#   np = rep(0, NP)
#   for (ip in 1:N) {
#      xp[(ip - 1) %% NP + 1] = xp[(ip - 1) %% NP + 1] + xx[ip]
#      np[(ip - 1) %% NP + 1] = np[(ip - 1) %% NP + 1] + 1
#    }

    ## xp: sum of time series elements with distance NP
    ## xn: number of summands of xp
    if (N%%NP == 0){
      xxp = array(xx, dim=c(NP, N/NP ))
      xxn = array(1, dim=c(NP, N/NP))
    } else {
     xxp = array(c(xx, rep(0, NP-N%%NP)), dim=c(NP, N/NP + 1))
     xxn = array(c(rep(1, N), rep(0, NP-N%%NP)), dim=c(NP, N/NP +1))
    }
    xp = rowSums(xxp)
    xn = rowSums(xxn)
    ## Fourier Transform of xp and xn
    XP = fft(xp)
    X0 = fft(xn)
    ## Periodograms of xp and xn
    PP = abs(XP) ^ 2 / NP ^ 2
    P0 = abs(X0) ^ 2 / NP ^ 2

    ## Correlation function of xp
    R=Re(ifft(PP)) / Re(ifft(P0))
    ## Fourier transform of the correlation function -> Periodogram
    P=Re(fft(R)) / NP
    PT = P[1 : (NP / 2 + 1)]
    ## Max. of the Periodogram
    idx = which.max(PT)
    if (idx > 1) {
      amp = max(PT) * NP
      if (amp > ampm) {
        ampm = amp
        ftm = (idx - 1) / (NP * delta_t)
      }
    }
  }

  if (names) names(ftm) <- "Optimized Frequency "
  return(ftm)
}

