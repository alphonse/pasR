absorb <- function(I, I0 = NULL, P, cal) {
  # retrieve calibration coefficients
  m <- coef(cal)[2]/1e-8
  b <- coef(cal)[1]/1e-8
  # calculate absorbance; for pre-subtracted data leave I0 blank
  if (is.null(I0)) abs <- I/P
  else abs <- (I-I0)/P
  (abs - b)/m
}