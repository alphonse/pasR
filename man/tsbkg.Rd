\name{tsbkg}
\alias{tsbkg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time Series Background Extraction
}
\description{
Extracts background measurements from spectral time series data in the example format.
}
\usage{
tsbkg(X, noAvg = 3, save = TRUE, file = 'tsbkg.txt', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A dataframe containing the columns 'Wavelength (nm)', 'Intensity (mV)', 'Type', where 'Type' specifies I or I0.}
  \item{noAvg}{The number of background data points to average (i.e. the number of places back in time included in the calculation).}
  \item{save}{A boolean indicating whether or not to write the extracted data to a text file.}
  \item{file}{A string indicating the file name if \code{save == TRUE}.}
  \item{\code{...}}{Other arguments passed to \code{write.table}.}
}

\value{
  \item{I0}{A dataframe with rows corresponding to the measurment wavelengths and columns corresponding to the mean background intensities for each blank period.}
}

\author{
  Al Fischer
  al@uga.edu
}
\seealso{
\code{\link{tssamp()}}
\code{\link{wavcols()}}
}
\examples{
data(ts)
colnames(ts) <- c('Time', 'Wavelength (nm)', 'Intensity (mV)', 'SD (mV)', 'Type') # columns MUST have these names!
I0 <- tsbkg(ts, noAvg = 3)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ photoacoustic }
\keyword{ broadband }
\keyword{ time series }

