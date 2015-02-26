plotts <- function(X) {
  manipulate(
    {
    tsplot <- ggplot(aes(x = Time, y = `Absorbance (/Mm)`), data = X) +
      geom_line(aes(color = `Wavelength (nm)`)) + theme_bw()
    tsplot + scale_x_datetime(limits = as.POSIXct(c(xmin, xmax), origin = "1970-01-01")) + 
      ylim(ymin, ymax)
  },
  ymin=slider(0, max(X$`Absorbance (/Mm)`)*2, 
              step=signif(max((X$`Absorbance (/Mm)`)-min(X$`Absorbance (/Mm)`))/100, digits=2), 
              initial = min(X$`Absorbance (/Mm)`)),
  xmin=slider(as.numeric(min(X$Time)), as.numeric(max(X$Time)),
              step = 1, 
              initial = as.numeric(min(X$Time))),
  ymax=slider(0, max(X$`Absorbance (/Mm)`)*2, 
              step=signif(max((X$`Absorbance (/Mm)`)-min(X$`Absorbance (/Mm)`))/100, digits=2), 
              initial = max(X$`Absorbance (/Mm)`)),
  xmax=slider(as.numeric(min(X$Time)), as.numeric(max(X$Time)), 
              step = 1, 
              initial = as.numeric(max(X$Time)))
  )
}