#' ECGs of 112 Fibrillating Murine Hearts
#'
#' The data set contains 112 murine ecgs.
#' The ecgs were obtained from seven mice, of which four were female (aged 112 or 113 weeks)
#' and three were male (aged 91 weeks).
#' The hearts were attached to a Langendorff perfusion system.
#' The electrocardiograms were acquired from an electrode near the left ventricle and an electrode
#' attached to the wall of the perfusion reservoir. The extracted data contain one second of
#' arrhythmia before the attempt of photodefibrillation. For ecg, the mean value was subtracted.
#' In order to remove traces of the 50 Hz utility frequency  a fifth order bandpass
#' Butterworth filter with a cutoff frequency of f_c = 45 Hz has been applied.
#'
#'
#' @format A matrix of 112 columns and 1000 rows:
#' Each column contains a single murine ECG.
#' The ECGs are recorded for 1 second and sampled with 1 kHz.
#' The data set is a multivariate time series with 1000 observations on 112 variables. The object is of class "mts".
#' @source  The data set was recorded by Laura Diaz and Lina el Shareif
#' (Max-Planck-Institute for Dynamics and Self-Organisation, Goettingen, Germany, 2023). It is discussed in detail in: OUR PAPER.
#' @examples
#' require(graphics)
#' ii <- 1
#' plot(MiceFibECGs[ ,ii], xlab = "Time [s]", ylab = "ECG", main = colnames(MiceFibECGs)[ii])
#'
"MiceFibECGs"
