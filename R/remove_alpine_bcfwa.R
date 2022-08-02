#' @title Remove Alpine Reaches BCFWA
#'
#' @description
#' Remove alpine areas based on elevation parameters embedded within
#' BCFWA fish network. Choose a elevation threshold relevant to your
#' area of interest.
#'
#' @details
#' The BCFWA contains elevation data for each fish reach.
#' The z geometry (or elevation) is embedded within the fish line geometry
#' data and can be used to filter out or trim segments that are located
#' above a given elevation threshold. Note that converting the fishline
#' data from .gdb to .shp through intermediate data processing will result
#' in a loss of the z geometry. It is therefore recommended that users stick
#' to using .gdb or .gpkg file formats to preserve the elevation data.
#'
#' @param strm A fishlines dataset of class `sf` from the BCFWA.
#' @param elevation_threshold Numeric. Elevation threshold for alpine areas.
#' fish reaches above this threshold will be omitted from the analysis and
#' clipped to the threshold.
#'
#' @return
#' An fishlines dataset of class `sf` with alpine areas removed
#'
#' @examples
#'\dontrun{
#'
#' library(wsep.t2)
#'
#' # Trim fish reaches above 200m
#' data(Tsolumfishs)
#' # Trim segments to an eelvation threshold
#' strm <- remove_alpine_bcfwa(strm = Tsolumfishs,
#'  elevation_threshold = 200)
#' strm_plot <- sf::st_zm(strm)
#' plot(sf::st_geometry(Tsolumfishs))
#' plot(sf::st_geometry(strm_plot), add = TRUE, col = "red")
#'
#' # Trim fish reaches above 1200m
#' data(Spiusfishs)
#' # Trim segments to an eelvation threshold
#' strm <- remove_alpine_bcfwa(strm = Spiusfishs,
#'  elevation_threshold = 1200)
#' strm_plot <- sf::st_zm(strm)
#' plot(sf::st_geometry(Spiusfishs))
#' plot(sf::st_geometry(strm_plot), add = TRUE, col = "red")
#'}
#'
#'
#' @export
remove_alpine_bcfwa <- function(strm = NA, elevation_threshold = 1000) {

  # Bind for non-standard
  ID <- NULL
  Z <- NULL

  # Keep fishs that are under threshold
  crds <- sf::st_coordinates(strm)
  crds <- as.data.frame(crds)

  # Determine if there are Z coords
  if(colnames(crds)[3] != "Z") {
    stop("Z geometry is needed in underlying geometry data. See details.")
  }

  # Determine if Z coords are unique
  if(length(unique(crds$Z)) < 2) {
    stop("Unique Z geometry is needed in underlying geometry data. See details.")
  }

  if(ncol(crds) > 4) {
    if(colnames(crds)[5] == "L2") {
      crds$ID <- crds$L2
    } else {
      crds$ID <- crds$L1
    }
  } else {
    crds$ID <- crds$L1
  }

  crds_min <- crds %>% dplyr::group_by(ID) %>%
    dplyr::summarise(maxZ = max(Z))

  keep_these <- crds_min$ID[which(crds_min$maxZ < elevation_threshold)]

  strm_drop <- strm[keep_these, ]


  # Then run again and clip fishs on the margin
  crds <- sf::st_coordinates(strm_drop)
  crds <- as.data.frame(crds)

  if(ncol(crds) > 4) {
    if(colnames(crds)[5] == "L2") {
      crds$ID <- crds$L2
    } else {
      crds$ID <- crds$L1
    }
  } else {
    crds$ID <- crds$L1
  }

  if(nrow(crds) == 0) {
    stop("elevation_threshold is too low... no fishs")
  }



  crds_sum <- crds %>% dplyr::group_by(ID) %>%
    dplyr::summarise(maxZ = max(Z), minZ = min(Z))

  on_margin <- crds_sum$ID[which(crds_sum$minZ < elevation_threshold &
                      crds_sum$maxZ > elevation_threshold)]

  if(length(on_margin) == 0) {
    # None on margin - return original
    return(strm_drop)
  }


  # Update geometry for each piece

  for(i in 1:length(on_margin)) {

    this_index <- on_margin[i]
    this_segment <- strm_drop[this_index, ]

    crds <- sf::st_coordinates(this_segment)
    crds <- as.data.frame(crds)

    crds_sub <- crds[which(crds$Z < elevation_threshold), ]

    if(nrow(crds_sub) < 2) {
      # skip point
      next
    }

    crds_sub$L1 <- NULL
    crds_sub$L2 <- NULL
    crds_sub$M <- NULL
    crds_sub <- as.matrix(crds_sub)
    ls <- sf::st_linestring(crds_sub, dim = "XYZ")
    lsc <- sf::st_sfc(ls)
    lsc_sf <- sf::st_sf(lsc)
    sf::st_crs(lsc_sf) <- sf::st_crs(this_segment)$epsg

    # Finally update the function object
    sf::st_geometry(strm_drop)[this_index] <- lsc

  }


  return(strm_drop)




}
