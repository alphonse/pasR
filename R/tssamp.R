# Extract sample values from time series data from 'PASAutomatic' LabVIEW program

tsSamp <- function(X, save = TRUE, file = './proc/tsSamp.txt', ...) {
  # colnames(X)<-c('Time', 'Wavelength (nm)', 'Intensity (mV)', 'SD (mV)', 'Type')
  wv <- unique(X$`Wavelength (nm)`)
  l  <- length(wv)
  ind  <- c(1, which(diff(X$Type == 'I') == -1), nrow(X))
  I <- data.frame()
  for (i in 1:(length(ind)-1)) {
      if (i == 1) {
        I[ind[i]:ind[(i+1)], i] <- X$`Intensity (mV)`[ind[i]:ind[(i+1)]]
      }
      else {
        I[(ind[i]+1):ind[(i+1)], i-1] <- X$`Intensity (mV)`[(ind[i]+1):ind[(i+1)]]
      }
  }
  
  if (save == TRUE) {
    if (file == './proc/tsSamp.txt') {
      write.table(I, file='./proc/tsSamp.txt', row.names = FALSE, ...)
    }
    else {
      write.table(I, file = file, row.names = FALSE, ...)
    }
  }
  I
}