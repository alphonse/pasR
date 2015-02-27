wavcols <- function(x, wavelengths = c(301, 314, 364, 405, 436, 546, 578, 660), colors = c('plum1', 'orchid1', 'darkorchid1', 'purple', 'blue', 'forestgreen', 'goldenrod3', 'red')) {
  colors[which(wavelengths %in% unique(x))]
}
