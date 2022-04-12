#' @title UTM Projection
#'
#' @description
#' Project data layer to the local UTM zone
#'
#' @details
#' Since many of the `wsep.t2` processing operations require working with
#' real-world distances in units of meters, it is necessary to ensure that
#' the spatial data layers are projected to the a local UTM zone. This function
#' determines the appropriate UTM zone based on the extent of the data layer
#' and then transforms the data to that projection.
#'
#' @param data a spatial object class `sf`.
#' @param epsg optional EPSG (spatial reference number) for the local UTM zone.
#' If not set, the EPSG will be inferred from the extent of the data layer.
#'
#' @return
#' A spatial object class `sf` projected to the appropriate UTM zone.
#'
#' @examples
#'\dontrun{
#' library(wsep.t2)
#' data(TsolumStreams)
#' strm <- utm_projection(data = TsolumStreams)
#' sf::st_crs(strm)$epsg
#'}
#'
#'
#' @export
utm_projection <- function(data = NA, epsg = NA) {


  # Determine if data can be projected
  if(!(all(class(data) == c("sf", "data.frame")))) {
    stop("data must be of class sf and data.frame")
  }

  # Get the current epsg code
  current_epsg <- sf::st_crs(data)$epsg

  if(is.na(current_epsg)) {
    stop("data must be imported with a known projection and valid epsg")
  }

  # If epsg code is set then use it
  if(!(is.na(epsg))) {
    if(class(epsg) != "numeric") {
      stop("epsg must be of class numeric (e.g., 32610)")
    }
    data_trans <- sf::st_transform(data, epsg)
    return(data_trans)
  }

  # If epsg code is not set determine it based on centroid
  bbox <- sf::st_as_sfc(sf::st_bbox(data))

  # Transform to lat long
  bbox <- sf::st_transform(bbox, 4326)
  cent <- sf::st_centroid(bbox)
  long <- sf::st_coordinates(cent)[1]

  # UTM zone from longitude
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }

  utm <- long2UTM(long = long)
  utm <- as.character(utm)

  if(nchar(utm) == 1) {
    utm <- paste0("0", utm)
  }

  epsg_new <- paste0("269", utm)
  epsg_new <- as.numeric(epsg_new)

  data_trans <- sf::st_transform(data, epsg_new)
  return(data_trans)


}
