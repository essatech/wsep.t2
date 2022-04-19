#' Spius Roads
#'
#' Sample road lines dataset for the Spius River watershed.
#' Used in a tutorial for the `wsep.t2`
#' (WSEP Tier II) R-package.
#'
#' @docType data
#'
#' @usage data(SpiusRoads)
#'
#' @format A road lines data set of class `sf`.
#' \describe{
#'   \item{TRANSPORT_LINE_ID}{A unique identifier for each road line segment}
#' }
#'
#' @keywords datasets
#'
#'
#' @examples
#' \donttest{
#' require(sf)
#' data(SpiusRoads)
#' head(SpiusRoads)
#' # Plot the streams with stream order
#' plot(st_geometry(SpiusRoads))
#' }
"SpiusRoads"
