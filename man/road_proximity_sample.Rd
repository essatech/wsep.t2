% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/road_proximity_sample.R
\name{road_proximity_sample}
\alias{road_proximity_sample}
\title{Road Proximity Sample}
\usage{
road_proximity_sample(
  n = 20,
  strm = NA,
  roads = NA,
  buffer_s1_m = 20,
  buffer_s2_m = 40,
  buffer_crossings_m = 100,
  small_strm_segment_m = 50,
  stream_order = "STREAM_ORDER"
)
}
\arguments{
\item{n}{Sample size n for each stratum. Defaults to 20.}

\item{strm}{A streamline data set of class \code{sf} from the BCFWA. Run \code{utm_projection()} first to ensure projection is in meters.}

\item{roads}{A road network data set of class \code{sf}. Run \code{utm_projection()} first to ensure projection is in meters.}

\item{buffer_s1_m}{Numeric. Buffer size (in meters) for Strata 1 streams < 3rd order. Defaults to 20 m.}

\item{buffer_s2_m}{Numeric. Buffer size (in meters) for Strata 2 streams >= 3rd order. Defaults to 40 m.}

\item{buffer_crossings_m}{Numeric. Exclusion buffer size (in meters) to stream-road crossings to exclude sample sites from Site Type A. Defaults to 100 m.}

\item{small_strm_segment_m}{Numeric. Remove small stream-road buffered segments unless they are near a switch-back (determined manually and added at end). Default small segment size is set to 50 m.}

\item{stream_order}{String. Column name for stream order. Defaults to \code{STREAM_ORDER}. Rename as needed.}
}
\value{
A object of class \code{sf} of sample features representing sample sites.
}
\description{
Road Proximity Sample for Site Type B
}
\details{
WSEP Tier II Road Proximity Sample for Site Type B
\itemize{
\item{Apply a buffer (20m for < 3rd order and 40 m for ≥ 3rd order) streams.}
\item{Taking the intersection of roads and this buffer}
\item{Removing any stream crossings (Site Type A), by excluding any cases within 100 m of a crossing to avoid double counting}
\item{Removing any segments < 50m in length unless they are near a switch-back (determined by manual review of the map). Switch-back determination is an optional manual step not automated in R code}
\item{Providing the start point of the segment as well as the segment length and strata id (< 3rd order vs. ≥ 3rd order) and mapping the entire segment on the field maps to facilitate sampling}
\item{Generate a random sample from the Site Type B list the strata with < 3rd order streams}
\item{ Append the complete list of Site Type B from the ≥ 3rd order strata }
\item{ Create a field checklist with at least the following fields: unique identifier (e.g., WatershedName_SD_B_001), coordinates of start point and end point, segment length, and Strata }
}
Note that there is no repeat sampling or replacement. If the sample size \code{n} exceeds the number of segments available for a stratum, then only the limited number of available locations will be returned.
}
