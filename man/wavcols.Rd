\name{wavcols}
\alias{wavcols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filter Colors by Wavelength
}
\description{
Choose colors for each wavelength in a spectrum based on the wavelengths used.
}
\usage{
wavcols(x, wavelengths = c(301, 314, 364, 405, 436, 546, 578, 660), colors = c("plum1", "orchid1", "darkorchid1", "purple", "blue", "forestgreen", "goldenrod3", "red"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector of wavelengths for which there is data.
}
  \item{wavelengths}{
A numeric vector of possible wavelengths.
}
  \item{colors}{
A character vector of color names for each value in \code{wavelengths}}
}

\value{
A character vector containing the colors to use.
}

\author{
Al Fischer <al@uga.edu>
}

\examples{
data(ambient)
wavcols(ambient[, 1])
}
