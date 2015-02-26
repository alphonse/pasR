\name{plotts}
\alias{plotts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactively Plot Time Series Data
}
\description{
Plots time series data from a photoacoustic instrument interactively.
}
\usage{
plotts(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A data frame with the columns "Time" (\code{\link{as.POSIXct()}}), "Absorbance (\ Mm)", and "Wavlength (nm)".
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An interactive plot of absorption vs. time.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Al Fischer <al@uga.edu>
}

\examples{
data(ts)
data(power)
colnames(ts) <- c('Time', 'Wavelength (nm)', 'Intensity (mV)', 'SD (mV)', 'Type')
I0 <- tsbkg(ts, noAvg = 3)
I  <- tsSamp(ts)
subtracted <- apply(mapply(I, I0[, 2:ncol(I0)], FUN = "-"), 1, na.omit)
ts$`Absorbance (/Mm)` <- absorb(I = subtracted, P = c(power[, 2], 1131, 703), cal = cal)

ts$Time <- as.POSIXct(ts$Time)
ts$`Wavelength (nm)` <- as.factor(ts$`Wavelength (nm)`)
plotts(ts)
}
