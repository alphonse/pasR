calibrate <- function(cal.file, ...) {
  # import and fit data
  if (class(cal.file) == "data.frame") calDat <- cal.file
  else {
    filepath <- cal.file
    calDat <- na.omit(read.csv(filepath, ...))
  }
  cal <- lm(normalized ~ alpha, data = calDat)
  
  # plot data and fit
  magplot(
    x=calDat$alpha,
    y=calDat$normalized,
    xlab=expression(paste(alpha, ' (c',m^-1,')')),
    ylab="Normalized Signal (V/W)",
    xaxs='i',
    yaxs='i'
    )
  abline(cal)
  legend('topleft', 
         legend = c(paste("m = ", signif(coef(cal)[2], digits = 4)),
                    paste("b = ", signif(coef(cal)[1], digits = 4)),
                    paste('R2 = ', signif(summary(cal)$r.squared, digits = 4))
                    ),
                    bty = 'n'
  )
  cal
}