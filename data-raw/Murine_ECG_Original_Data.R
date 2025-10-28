## code to prepare `Murine_ECG_Original_Data` dataset goes here


library("gsignal")
fs = 1000
bwf = butter(5, 45/(fs/2) )


LData = "data-raw/Murine_ECG_Original_Data.RData"

load(LData)

colnames(LAURAS_DATA)=substring(colnames(LAURAS_DATA), 10,16)


MiceFibECGs <- LAURAS_DATA

#for (i in 1:(dim(MiceFibECGs))[2]) MiceFibECGs[,i] <- notch_50_l1000(MiceFibECGs[,i])
for (i in 1:(dim(MiceFibECGs))[2])
  {
  MiceFibECGs[,i] <- MiceFibECGs[,i] - mean(MiceFibECGs[,i])
  MiceFibECGs[,i] <- filtfilt(bwf, MiceFibECGs[,i])
  MiceFibECGs[,i] <- MiceFibECGs[,i] - mean(MiceFibECGs[,i])
  }

remove(LAURAS_DATA)

usethis::use_data(MiceFibECGs, overwrite = TRUE)

