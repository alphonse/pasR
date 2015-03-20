\name{angfit}
\alias{angfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Angstrom Exponent
}
\description{
A wrapper for \code{\link{nls}} to calculate Angstrom exponents}
\usage{
angfit(X, start = list(ang=2, beta=1e5), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A data frame of wavelength in column 1 and absorption in column 2}
  \item{start}{The initial guess for the fit}
  \item{...}{Additional arguments to be passed to \code{\link{nls}}}
}
\details{
See \code{\link{nls}}.
}
\value{
\code{\link{nls}}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Al Fischer <al@uga.edu>}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{nls}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ angstrom }