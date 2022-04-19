#' Spius Streams
#'
#' Sample streamlines dataset for the Spius River in
#' British Columbia. Used in a tutorial for the `wsep.t2`
#' (WSEP Tier II) R-package.
#'
#' @docType data
#'
#' @usage data(SpiusStreams)
#'
#' @format A streamlines data set of class `sf`.
#' \describe{
#'   \item{LINEAR_FEATURE_ID}{A unique identifier for
#'   each streamline segment}
#'   \item{EDGE_TYPE}{Edge type codes from the BCFWA.
#'   Useful for determining lentic or lotic habitat}
#'   \item{STREAM_ORDER}{Strahler (1952) stream order
#'   for each segment}
#'   \item{GNIS_NAME}{Stream name}
#' }
#'
#' @keywords datasets
#'
#'
#' @examples
#' \donttest{
#' require(sf)
#' data(SpiusStreams)
#' head(SpiusStreams)
#' # Plot the streams with stream order
#' plot(SpiusStreams["STREAM_ORD"])
#' }
"SpiusStreams"
