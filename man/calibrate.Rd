\name{calibrate}
\alias{calibrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate calibration factors and a plot calibration curve from PAS data.
}
\description{
  Calculates the absorption of a sample give \eqn{I} and \eqn{I0} values from a photoacoustic spectrometer.
}
\usage{
  calibrate(cal.file, ...)
}
\arguments{
  \item{cal.file}{A path to a \code{csv} file with the columns are \code{alpha} and \code{normalized} OR a data frame from the imported csv.}
  \item{...}{Addtional arguments passed to \code{\link{read.csv()}} or \code{\link{magplot()}}}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  A function representing the linear fit of the power normalized signal to extinction.
}

\references{
  
}
\author{
  Al Fischer <al@uga.edu>
}
  
\examples{
data(cal)
cal <- calibrate(cal)
}