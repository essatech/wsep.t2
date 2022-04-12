#' Tsolum Roads
#'
#' Sample road lines dataset for the Tsolum River watershed near
#' Courtenay British Columbia. Used in a tutorial for the `wsep.t2`
#' (WSEP Tier II) R-package.
#'
#' @docType data
#'
#' @usage data(TsolumRoads)
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
#' data(TsolumRoads)
#' head(TsolumRoads)
#' # Plot the streams with stream order
#' plot(st_geometry(TsolumRoads))
#' }
"TsolumRoads"
