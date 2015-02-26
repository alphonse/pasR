\name{absorb}
\alias{absorb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate Absorption
}
\description{
Calculates the absorption of a sample give \eqn{I} and \eqn{I0} values from a photoacoustic spectrometer.
}
\usage{
  absorb(I0, I, P, cal)
}
\arguments{
  \item{I0}{A numeric vector of the background intensity}
  \item{I}{A numeric vector of the sample intensity}
  \item{P}{A numeric vector of the illumination power}
  \item{cal}{An R function from the output of \code{calibrate}}
}
  
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  A vector containing the caluclated absorption in 1/mM
}

\references{
    
}
\author{
  Al Fischer <al@uga.edu>
}
\seealso{
    \code{\link{tsbkg()}}, \code{\link{tssamp()}}, \code{\link{calibrate()}}
  }
  
\examples{
data(bkg)
data(power)
data(ambient)
data(cal)

# calibrate:
cal <- calibrate(cal)
# get wavelengths:
wavelength <- bkg[, 1]
# calculate spectrum: 
spec <- absorb(I = ambient[, 2], I0 = bkg[ , 2], P = power[, 2], cal = cal)
# plot:
plot(wavelength, spec, type = 'b')
}

\keyword{ photoacoustic }
\keyword{ absorption }% __ONLY ONE__ keyword per line