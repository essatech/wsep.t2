#' @title Remove Lentic Reaches BCFWA
#'
#' @description
#' Remove lakes and lentic waterbodies within in BCFWA stream network
#'
#' @details
#' Lakes and lentic wetlands should be removed from the streamline
#' sampling frame to avoid having a sample point fall in the middle of a
#' stationary water body. If the streamline network being used originates from
#'  the BC Freshwater Atlas then it is possible to the `EDGE_TYPE` column to
#'  keep or retain lotic (stream) reaches and drop lotic (lake/wetland areas).
#'
#' @param strm A streamlines dataset of class `sf` from the BCFWA.
#' @param EDGE_TYPE String. Column name for the `EDGE_TYPE` in the streamline
#' dataset. See BCFWA User Guide for details.
#'
#' @return
#' An streamlines dataset of class `sf` with lotic reaches removed
#'
#' @examples
#'\dontrun{
#' library(wsep.t2)
#' data(TsolumStreams)
#' # Remove lotic (lake) reaches
#' strm <- remove_lentic_bcfwa(strm = TsolumStreams,
#'  EDGE_TYPE = "EDGE_TYPE")
#' strm_plot <- sf::st_zm(strm)
#' # mapview::mapview(strm_plot)
#'}
#'
#'
#' @export
remove_lentic_bcfwa <- function(strm = NA, EDGE_TYPE = "EDGE_TYPE") {

  # Get edge type values
  edge_types <- strm[, EDGE_TYPE]
  # Drop geometry
  sf::st_geometry(edge_types) <- NULL

  edge_types <- edge_types[, 1]
  edge_types <- as.numeric(as.character(edge_types))

  # Drop lakes 1200 and connectors 1450
  keep <- which(!(edge_types %in% c(1200, 1450)))

  strm <- strm[keep, ]

  return(strm)

}
