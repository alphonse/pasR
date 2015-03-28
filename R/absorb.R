absorb <- function(I, I0 = NULL, P, cal) {
  # sort data for consistency
  I  <- arrange(I, lambda)
  I0 <- arrange(I0, lambda)
  P  <- arrange(P[, 1])
  # retrieve calibration coefficients
  m <- coef(cal)[2]
  b <- coef(cal)[1]
  # calculate absorbance; for pre-subtracted data leave I0 blank
  if (is.null(I0)) abs <- I/P[, 2]
  else abs <- (I-I0)/P[, 2]
  (abs - b)/m
}