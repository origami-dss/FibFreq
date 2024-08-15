## code to prepare `MiceFibEGCs` dataset goes here

#MiceFibEGCs  Namen - und spaltennamen müssen noch geändert werden.

LData = "/Users/annette/Desktop/BMPG/Laura Wettbewerb/LAURAS_DATA.RData"

load(LData)

colnames(LAURAS_DATA)=substring(colnames(LAURAS_DATA), 10,16)

LAURAS_DATA <- LAURAS_DATA[,!colnames(LAURAS_DATA) %in% c("M16_R25","M17_R41", "M17_R45", "M18_R38")]
MiceFibECGs <- LAURAS_DATA
MiceFibECGs <- ts(MiceFibECGs, start=0.001, deltat=0.001)

remove(LAURAS_DATA)

usethis::use_data(MiceFibECGs, overwrite = TRUE)
