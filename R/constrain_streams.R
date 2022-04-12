#' @title Constrain Streams
#'
#' @description
#' Constrains the streamline layer
#'
#' @details
#' There is some lack of correspondence between the tangible,
#' physical population of stream reaches and the BC Freshwater Atlas.
#' Two potential sources of non-correspondence are incomplete coverage
#' (i.e., there are streams in the landscape that do not have
#' corresponding mapped depictions in the 1:20K sample frame) and
#' over-coverage (i.e., there may be stream traces indicated that do
#' not correspond to flowing streams in the field, particularly at the
#' upper end of first order streams) (Stevens 2002). In order to minimize
#' the frequency of encountering a non-classified drainage (NCD) in the field,
#' the sample
#' frame for is constrained by:
#' \itemize{
#'  \item{removing 1st order streams that are less than 500 m in length and}
#'  \item{removing the upper 200 m  of remaining 1st order streams depicted by
#'  the 1:20K GIS layer}
#' }
#' This restriction will not eliminate NCDs,
#' particularly for interior watersheds where NCDs are not restricted to
#' the upper end of the first order streams.
#'
#' @param strm A streamlines dataset of class `sf` from the BCFWA.
#'
#' @return
#' An constrained streamlines dataset of class `sf`.
#'
#' @examples
#'\dontrun{
#' constrain_streams()
#'}
#'
#'
#' @export
constrain_streams <- function(strm = NA) {

  print("constrain_streams")
  x <- sum(sf::st_length(strm))

  return(x)
}
