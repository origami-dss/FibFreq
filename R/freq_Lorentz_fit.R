#' Frequency Estimation Using a Lorentz Fit to the Periodogram
#'
#' @description
#' This function is fitting a Lorentz function to the periodogram of the time series x.
#'
#' @param  x real valued array containing the values of the time series
#' @param delta_t  a real value, the sampling time, default value is delta_t = 1.0
#' @param welch_window  if TRUE, the time series will be multiplied with a Welch window before Fourier transform
#'
#' @return  a list containing the parameters of the Lorentz fit.
#' @export
#'
#' @examples
#' x1 = sin(0.11*2*pi*(1:100));
#' x2 = sin(0.11*2*pi*(1:100))+ 1.01*sin(0.17*2*pi*(1:100));
#' x3 = sin(0.10*2*pi*(1:100))+ sin(0.20*2*pi*(1:100));
#' freq_Lorentz_fit(x1)
#' freq_Lorentz_fit(x2)
#' freq_Lorentz_fit(x2, welch_window = TRUE)
#' freq_Lorentz_fit(x3)
#'
freq_Lorentz_fit <- function(x, delta_t = 1.0, welch_window = FALSE, f_min = 0, f_max = Inf, algorithm = "Whittle")
{
  if(!is.vector(x) & !is.numeric(x) & any(is.na(x)) & any(is.infinite(x))) stop("'x' must be non-infinite real-valued numeric vector")
  if (length(x) == 0L) stop("data series to short")
  if (!is.numeric(delta_t) & length(delta_t) != 1L & any(is.infinite(delta_t))) stop("'delta_t' must be finite numeric of length one")
  if (delta_t <= 0) stop("'delta_t' must be positive (>0)")
  if (length(welch_window) != 1L & !is.logical(welch_window)) stop("'welch_window' must be logical of length one")
  if (!is.numeric(f_min) & length(f_min) != 1L & f_min <=0)  stop("'f_min' must be must be positive numeric of length one")
  if (!is.numeric(f_max) & length(f_max) != 1L & f_max <=0)  stop("'f_max' must be must be positive numeric of length one")
  if (f_max <= f_min +1./length(x)/delta_t) stop("frequency range does not exist or does not contain a sampled frequency")
  if (!tolower(algorithm) %in% c("whittle" , "ls" )) stop("'algorithm' must be 'Whittle' or 'ls'")
  if (var(x)== 0)
  {
    warning("'x' does not contain fluctuations, returning NA");
    res <- list(freq_mode_periodogram = NA_real_, expressed_var = NA_real_, f_min = f_min, f_max = f_max)
  }

    P=Periodogram(x, delta_t = delta_t, welch_window = welch_window)
    freq=P$Frequency
    w <- which(freq > f_min & freq < f_max)
    freq <- freq[w]
    delta_f <- diff(freq[1:2])
    Pow <- P$Power[w]
    PP <- list(Frequency = freq, Power = Pow)
 #   Pow=Pow/sum(Pow)/delta_f
    ww=which.max(Pow)
    argmax_freq <-  freq[ww]
    Lorentzfit <- nls(Pow ~ normalized_cauchy(PP, location, scale),
                     start = list(location = argmax_freq, scale = 0.002),
                     lower = list(location = f_min, scale=0.000001),
                     upper = list(location = f_max, scale=4),
                     algorithm = "port", trace=TRUE)

    result <- coef(Lorentzfit)
    Lorentz_freq <-  result[1]
    Lorentz_scale <- result[2]
#    Lorenz_freq <-  result[1,1]
#    Lorenz_freq_se <-  result[1,2]
#    Lorenz_scale <- result[2,1]
#    Lorenz_scale_se <- result[2,2]

#    res <- list(Lorenz_freq = Lorenz_freq, Lorenz_scale = Lorenz_scale, Lorenz_freq_se = Lorenz_freq_se, Lorenz_scale_se = Lorenz_scale_se)
    if (tolower(algorithm) == "ls") res <- list(Lorentz_freq = Lorentz_freq,  Lorentz_scale = Lorentz_scale, algorithm = "ls") else{

      Res_optim <- optim(c(location = Lorentz_freq, scale =Lorentz_scale), whittle_loglik, gr=NULL, method = "L-BFGS-B", lower = c(min(freq),0.00001), upper = c(max(freq), 4.0), P=PP)
      optimized_param= Res_optim$par
      res <- list(Lorentz_freq = optimized_param[1],  Lorentz_scale = optimized_param[2], algorithm = "Whittle")
      }
  return(res)
}

#
# AUXILIARY FUNCTIONS
#
normalized_cauchy <- function(P, location, scale)
  {
  Pow <- P$Power
  freq <- P$Frequency
  Dcauchy <- dcauchy(freq, location, scale)
  Dcauchy <- Dcauchy *sum(Pow)/sum(Dcauchy)
  return(Dcauchy)
  }

scaled_cauchy <- function(freq, fac, location, scale)
{
  Scauchy <- fac * dcauchy(freq, location, scale)
  return(Scauchy)
}

whittle_loglik <- function(param,  P)
{
  #
  #   Whittle Log-likelihood
  #

  normalized_cauchy=normalized_cauchy(P, param[1], param[2])
#  location = param[1]
#  scale = param [2]
  Pow <- P$Power
  freq <- P$Frequency
  if(length(freq) >1)  delta_f <- diff(freq[1:2]) else delta_f = 1

  #
  #Spectral Density:
  #
#  Dcauchy <- dcauchy(freq, location, scale)
#  Dcauchy <- Dcauchy *sum(Pow)/sum(Dcauchy)
  #
  #  Calculate sigma^2
  #‚
  #  Whittle Log-likelihood
  #
  loglik <- delta_f*(sum(log(normalized_cauchy)) + sum(Pow/normalized_cauchy))
  return(loglik)
}


whittle_loglik_a <- function(param,  P)
{
  #
  Pow <- P$Power
  freq <- P$Frequency
  scaled_cauchy = scaled_cauchy(freq, param[1], param[2], param[3])
  #  fac = param[1]
  #  location = param[2]
  #  scale = param [3]

  if(length(freq) >1)  delta_f <- diff(freq[1:2]) else delta_f = 1

  #  Whittle Log-likelihood
  #
  loglik <- delta_f*(sum(log(scaled_cauchy)) + sum(Pow/scaled_cauchy))
  return(loglik)
}


