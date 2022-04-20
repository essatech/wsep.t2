#' @title Site Type B Sample
#'
#' @description
#' Special sampling procedure for Site Type B
#'
#' @param strm A filtered and constrained streamlines dataset of class `sf` from the BCFWA.
#' @param roads A roads dataset of class `sf` (utm projection to match strm).
#' @param stream_order_column String. Column name for the stream order column.
#' @param buffer_small_m Numeric (meters). Buffer size for order 1 and 2 streams.
#' Defaults to 20 m.
#' @param buffer_large_m Numeric (meters). Buffer size for stream orders 3 and up.
#' Defaults to 40 m.
#' @param small_segment_exclude Numeric (meters). Segment size to exclude.
#' Defaults to 50 m.
#' @param group_name String. Column name for the strata to sample across
#' @param n Sample size n for each group (defaults to global if group is NA).
#'
#' \enumerate{
#'   \item Apply a buffer (20m for < 3rd order and 40 m for ≥ 3rd order) streams.
#'   \item Taking the intersection of roads and this buffer.
#'   \item Removing any stream crossings (site Type A), by excluding any cases
#'    within 100m of a crossing to avoid double counting.
#'   \item Removing any segments < 50m in length unless they are near a switch-back
#'   (determined by manual review of the map).
#'   \item Providing the start point of the segment as well as the segment length
#'   and strata id (< 3rd order vs. ≥ 3rd order) and mapping the entire segment
#'   on the field maps to facilitate sampling.
#'   \item Generate a random sample from the Site Type B list the strata with < 3rd order streams.
#'   \item Append the complete list of Site Type B from the ≥ 3rd order strata.
#'   \item Create a field checklist with at least the following fields:
#'   unique identifier (e.g., WatershedName_SD_B_001), coordinates of start
#'   point and end point, segment length, and Strata.
#' }
#'
#' @return
#' A object of class `sf` of sample features.
#'
#' @export
site_type_b_sample <- function(strm = NA,
                               roads = NA,
                               stream_order_column = "STREAM_ORDER",
                               buffer_small_m = 20,
                               buffer_large_m = 40,
                               small_segment_exclude = 50,
                               group_name = NA,
                               n = 20) {

  # Add missing column to streams
  vals <- strm[, stream_order_column]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]

  strm$tmp_strm_order <- vals
  strm$tmp_id <- 1:nrow(strm)


  # Make stream crossing layer
  sc <- sf::st_intersection(roads, strm)

  # Need to fix multi point
  sc1 <- sc[which(sf::st_geometry_type(sc) =="POINT"),]
  sc2 <- sc[which(sf::st_geometry_type(sc) =="MULTIPOINT"),]
  sc2 <- sf::st_cast(sc2, "POINT")
  sc <- rbind(sc1, sc2)




  # start with 20m buffer first
  ssub <- strm[which(strm$tmp_strm_order < 3), ]
  buff <- sf::st_buffer(ssub, buffer_small_m)
  buff <- buff %>% dplyr::group_by() %>% dplyr::summarise()

  # (b) taking the intersection of roads and this buffer,
  rint <- sf::st_intersection(roads, buff)
  # table(sf::st_geometry_type(rint))
  rint <- sf::st_cast(rint, "LINESTRING")
  rint$tmp <- "samp"
  rint <- rint[,c("tmp")]

  # (c) removing any stream crossings (site Type A),
  # by excluding any cases within 100m of a crossing to avoid double counting,
  sc2_100 <- sf::st_buffer(sc2, 100)
  sc2_100 <- sc2_100 %>% dplyr::group_by() %>% dplyr::summarise()
  sdiff <- sf::st_difference(rint, sf::st_union(sc2_100))


  # (d) removing any segments <50m in length unless they are
  # near a switch-back (determined by manual review of the map),
  sdiff$length_m <- sf::st_length(sdiff)
  sdiff$length_m <- as.numeric(sdiff$length_m)
  skeep <- sdiff[which(sdiff$length_m > small_segment_exclude), ]


  #--------------------------------------------------
  # Then work through larger streams
  # start with 20m buffer first
  ssub <- strm[which(strm$tmp_strm_order >= 3), ]
  buff <- sf::st_buffer(ssub, buffer_large_m)
  buff <- buff %>% dplyr::group_by() %>% dplyr::summarise()

  # (b) taking the intersection of roads and this buffer,
  rint <- sf::st_intersection(roads, buff)
  # table(sf::st_geometry_type(rint))
  rint <- sf::st_cast(rint, "LINESTRING")
  rint$tmp <- "samp"
  rint <- rint[,c("tmp")]

  # (c) removing any stream crossings (site Type A),
  # by excluding any cases within 100m of a crossing to avoid double counting,
  sc2_100 <- sf::st_buffer(sc2, 100)
  sc2_100 <- sc2_100 %>% dplyr::group_by() %>% dplyr::summarise()
  sdiff <- sf::st_difference(rint, sf::st_union(sc2_100))

  # (d) removing any segments <50m in length unless they are
  # near a switch-back (determined by manual review of the map),
  sdiff$length_m <- sf::st_length(sdiff)
  sdiff$length_m <- as.numeric(sdiff$length_m)
  skeep <- sdiff[which(sdiff$length_m > small_segment_exclude), ]


  mapview::mapview(skeep)








  # (e) providing the start point of the segment as well as the segment length, and mapping the entire segment on the field maps to facilitate sampling.


}
