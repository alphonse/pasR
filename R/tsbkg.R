# Extract background values from time series data from 'PASAutomatic' LabVIEW program

tsbkg <- function(X, noAvg = 3, save = TRUE, file = './proc/tsbkg.txt', ...) {
# colnames(X) <- c('Time', 'Wavelength (nm)', 'Intensity (mV)', 'SD (mV)', 'Type')
  wv <- unique(X$`Wavelength (nm)`)
  l  <- length(wv)
  i  <- which(diff(X$Type == 'I0') == -1)
  I0 <- X$`Intensity (mV)`[rep(i, each=l*noAvg)-(0:((l*noAvg)-1))]
  I0 <- matrix(rowMeans(matrix(I0, ncol = noAvg)), nrow = l)
  I0 <- as.data.frame(cbind(wv, I0), row.names = F)
  colnames(I0) <- c('Wavelength (nm)', paste('I0-', 1:(ncol(I0)-1), sep = ''))
  
  if (save == TRUE) {
    if (file == './proc/tsbkg.txt') {
      write.table(I0, file='./proc/tsbkg.txt', row.names = FALSE, ...)
    }
    else {
      write.table(I0, file = file, row.names = FALSE, ...)
    }
  }
  
  return(I0)
}