#' ECGs of 113 Fibrillating Murine Hearts
#'
#' The data set contains 113 murine ecgs.
#' The ecgs were obtained from seven mice, of which four were female (aged 112 or 113 weeks)
#' and three were male (aged 91 weeks).
#' The hearts were attached to a Langendorff perfusion system.
#' The electrocardiograms were acquired from an electrode near the left ventricle and an electrode
#' attached to the wall of the perfusion reservoir. The extracted data contain one second of
#' arrhythmia before the attempt of photodefibrillation.
#'
#'
#' @format A matrix of 113 columns and 1000 rows:
#' Each column contains a single murine ECG.
#' The ECGs are recorded for 1 second and sampled with 1 kHz.
#' The column names indicate the mouse ID and the trial number, i.e., "M15_R01" is the first trial of the mouse with ID no. 15.
#' The data set is a multivariate time series with 1000 observations on 113 variables. The object is of class "mts".
#' @source  The data set was recorded by Laura Diaz and Lina el Shareif
#' (Max-Planck-Institute for Dynamics and Self-Organisation, Goettingen, Germany, 2023). It is discussed in detail in: OUR PAPER.
#' @examples
#' require(graphics)
#' ii <- 1
#' plot(MiceFibECGs[ ,ii], xlab = "Time [s]", ylab = "ECG", main = colnames(MiceFibECGs)[ii])
#'
"MiceFibECGs"
