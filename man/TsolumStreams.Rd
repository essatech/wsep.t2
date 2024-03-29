% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TsolumStreams-data.R
\docType{data}
\name{TsolumStreams}
\alias{TsolumStreams}
\title{Tsolum Streams}
\format{
A streamlines data set of class \code{sf}.
\describe{
\item{LINEAR_FEATURE_ID}{A unique identifier for
each streamline segment}
\item{EDGE_TYPE}{Edge type codes from the BCFWA.
Useful for determining lentic or lotic habitat}
\item{STREAM_ORDER}{Strahler (1952) stream order
for each segment}
\item{GNIS_NAME}{Stream name}
}
}
\usage{
data(TsolumStreams)
}
\description{
Sample streamlines dataset for the Tsolum River near
Courtenay British Columbia. Used in a tutorial for the \code{wsep.t2}
(WSEP Tier II) R-package.
}
\examples{
\donttest{
require(sf)
data(TsolumStreams)
head(TsolumStreams)
# Plot the streams with stream order
plot(TsolumStreams["STREAM_ORDER"])
}
}
\keyword{datasets}
