\name{tssamp}
\alias{tssamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract Samples}
\description{
Extract sample data from a time series.
}
\usage{
tssamp(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{X}{A data frame with the columns "Time" (\code{\link{as.POSIXct()}}), "Intensity (mV)",  "Wavlength (nm)", and "Type", where "Type" indicates I (sample) or I0 (background).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing only the sample (I) values.
}

\author{
Al Fischer <al@uga.edu>
}

\seealso{
\code{\link{tsbkg()}}
\code{\link{wavcols()}}
}

\examples{
data(ts)
colnames(ts) <- c('Time', 'Wavelength (nm)', 'Intensity (mV)', 'SD (mV)', 'Type') # columns MUST have these names!
I  <- tsSamp(ts)
}
