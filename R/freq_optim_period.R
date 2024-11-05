#' Frequency Estimation Using the Optimized Period Technique
#'
#' @param x real valued array containing the values of the time series
#' @param delta_t a real value, the sampling time, default value is delta_t = 1.0
#' @param f_min a real value, the minimum frequency of the tested frequency range
#' @param f_max a real value, the maximum frequency of the tested frequency range
#'
#' @return a list containing the best-fitting frequency estimated with the ptimized Period Technique and the explained variance
#' @export
#'
#' @examples
#' x1 = sin(0.11 * 2*pi * (1:100));
#' x2 = sin(0.111* * 2*pi * (1:100)) + 1.01 * sin( 0.17 * 2*pi * (1:100));
#' freq_optim_period(x1)
#' freq_optim_period(x2)
#'
freq_optim_period <- function(x, delta_t = 1.0, f_min = 0 , f_max = Inf) {
  if (!is.vector(x) |
      !is.numeric(x) |
      any(is.na(x)) |
      any(is.infinite(x)))
    stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L)
    stop("data series to short")
  if (length(delta_t) != 1L |
      !is.numeric(delta_t) |
      is.infinite(delta_t) |
      delta_t <= 0)
    stop("'delta_t' must be positive finite numeric of length one")
  if (length(f_min) > 1L |
      !is.numeric(f_min) |
      is.na(f_min) |
      is.infinite(f_min) |
      f_min < 0)
    stop("'f_min' must be must be positive numeric of length one")
  if (length(f_max) > 1L |
      !is.numeric(f_max) |
      is.na(f_max) |
      f_max < 0)
    stop("'f_max' must be must be positive numeric of length one")
  if (f_max <= f_min + 1. / length(x) / delta_t)
    stop("frequency range does  not contain a sampled frequency")
  if (var(x) == 0)
  {
    res <- c(freq_optim = NA_real_, expressed_var = NA_real_)
    message("time series 'x' does not contain fluctuations, returning NA")
  } else
  {
    N <- length(x)
    result = array(c(0), dim=c(4,1+floor(N/2)))

    for (NP in floor(N/2):N) result[, NP +1 - floor(N/2) ] <- Periodogram_sampled(x, NP , delta_t)

    mp <- which.max(result[4, ])

    freq_optim <- result[1, mp]
    exp_variance <- result[4, mp]

    res <- c(freq_optim = freq_optim, exp_variance = exp_variance)
  }
  return(res)
}



ifft <- function(x) {
  Conj(fft(x)) / (2 * pi)
}

Periodogram_sampled <- function(x, assumed_ts_length, delta_t )
{
  N <- length(x)
  N_rem <- N %% assumed_ts_length
  xxp <- array(c(x, rep(0, assumed_ts_length - N_rem)), dim = c(assumed_ts_length, N / assumed_ts_length + 1))
  xxn <- array(c(rep(1, N), rep(0, assumed_ts_length - N_rem)), dim = c(assumed_ts_length, N / assumed_ts_length + 1))

  xp <- rowSums(xxp)
  xn <- rowSums(xxn)
  ## Fourier Transform of xp and xn
  XP <- fft(xp)
  XN <- fft(xn)

  ## Periodograms of XP and XN
  PP <- abs(XP) ^ 2 / assumed_ts_length ^ 2
  PN <- abs(XN) ^ 2 / assumed_ts_length ^ 2

  ## Correlation function of XP
  C_coarse_grained <- Re(ifft(PP)) / Re(ifft(PN))

  ## Fourier transform of the correlation function -> Periodogram of xp with respect to zero padding

  P <- Re(fft(C_coarse_grained))
  PT <- 2*P[2:(floor(assumed_ts_length / 2 )+ 1)]
  idx <- which.max(PT)

  freq_max = idx/ length(PT)/2/delta_t
  max_periodogram= PT[idx]
  exp_var = max_periodogram/var(x)/assumed_ts_length
  normalized_max_periodogram = max_periodogram *N /assumed_ts_length
  print(max(PT)/sum(PT))

  res = c(freq_max = freq_max, max_periodogram = max_periodogram, normalized_max_periodogram = normalized_max_periodogram, exp_var = exp_var )
  res = c(freq_max = freq_max, max_periodogram = max_periodogram/sum(PT), normalized_max_periodogram = normalized_max_periodogram, exp_var = exp_var )

  return (res)
}

# Originalversion von Holger
#
#
# freqest <- function(x, dt) {
#   ftm = 0.0
#   ampm = 0.0
#   N = length(x)
#   xx = x - mean(x)
#   for (NP in 2:N) {
#     xp = rep(0, NP)
#     np = rep(0, NP)
#     for (ip in 1:N) {
#       xp[(ip - 1) %% NP + 1] = xp[(ip - 1) %% NP + 1] + xx[ip]
#       np[(ip - 1) %% NP + 1] = np[(ip - 1) %% NP + 1] + 1
#     }
#     XP = fft(xp)
#     X0 = fft(np)
#     PP = abs(XP) ^ 2 / NP ^ 2
#     P0 = abs(X0) ^ 2 / NP ^ 2
#     R=Re(ifft(PP)) / Re(ifft(P0))
#     P=Re(fft(R)) / NP
#     PT = P[1 : (NP / 2 + 1)]
#     idx = which.max(PT)
#     if (idx > 1) {
#       amp = max(PT) * NP
#       if (amp > ampm) {
#         ampm = amp
#         ftm = (idx - 1) / (NP * dt)
#         #plot(PT)
#       }
#     }
#   }
#   ftm
# }
#
#
#
